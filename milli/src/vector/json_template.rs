//! Module to manipulate JSON templates.
//!
//! This module allows two main operations:
//! 1. Render JSON values from a template and a context value.
//! 2. Retrieve data from a template and JSON values.

#![warn(rustdoc::broken_intra_doc_links)]
#![warn(missing_docs)]

use serde_json::{Map, Value};

type ValuePath = Vec<PathComponent>;

/// Encapsulates a JSON template and allows injecting and extracting values from it.
#[derive(Debug)]
pub struct ValueTemplate {
    template: Value,
    value_path: Option<ValuePath>,
    array_path: Option<ArrayPath>,
}

#[derive(Debug)]
struct ArrayPath {
    repeated_value: Value,
    path_to_array: ValuePath,
    value_path_in_array: ValuePath,
}

/// Component of a path to a Value
#[derive(Debug, Clone)]
pub enum PathComponent {
    /// A key inside of an object
    MapKey(String),
    /// An index inside of an array
    ArrayIndex(usize),
}

impl PartialEq for PathComponent {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::MapKey(l0), Self::MapKey(r0)) => l0 == r0,
            (Self::ArrayIndex(l0), Self::ArrayIndex(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl Eq for PathComponent {}

impl PartialOrd for PathComponent {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for PathComponent {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (PathComponent::MapKey(left), PathComponent::MapKey(right)) => left.cmp(right),
            (PathComponent::MapKey(_), PathComponent::ArrayIndex(_)) => std::cmp::Ordering::Less,
            (PathComponent::ArrayIndex(_), PathComponent::MapKey(_)) => std::cmp::Ordering::Greater,
            (PathComponent::ArrayIndex(left), PathComponent::ArrayIndex(right)) => left.cmp(right),
        }
    }
}

/// Error that occurs when too few values were provided to a template for injection.
#[derive(Debug)]
pub struct MissingStaticValue;

/// Result of extracting subvalues from a value and a template.
pub struct ExtractedValues {
    /// Value extracted from the static location
    pub static_value: Option<Value>,
    /// Values extracted from the array
    pub array_values: Option<Vec<Value>>,
}

impl ExtractedValues {
    pub fn into_iter(self) -> impl Iterator<Item = Value> {
        self.static_value
            .into_iter()
            .chain(self.array_values.into_iter().flat_map(|array| array.into_iter()))
    }
}

/// Error that occurs when trying to parse a template in [`ValueTemplate::new`]
#[derive(Debug)]
pub enum TemplateParsingError {
    /// A repeat string appears inside a repeated value
    NestedRepeatString(ValuePath),
    /// A repeat string appears outside of an array
    RepeatStringNotInArray(ValuePath),
    /// A repeat string appears in an array, but not in the second position
    BadIndexForRepeatString(ValuePath, usize),
    /// A repeated value lacks a placeholder
    MissingPlaceholderInRepeatedValue(ValuePath),
    /// A repeated value contains multiple placeholders
    MultiplePlaceholdersInRepeatedValue(ValuePath, Vec<ValuePath>),
    /// Multiple repeat string appear in the template
    MultipleRepeatString(ValuePath, ValuePath),
    /// Multiple placeholder strings appear in the template
    MultiplePlaceholderString(ValuePath, ValuePath),
}

/// Error that occurs when [`ValueTemplate::extract`] fails.
#[derive(Debug)]
pub struct ExtractionError {
    /// The cause of the failure
    pub kind: ExtractionErrorKind,
    /// The context where the failure happened: the operation that failed
    pub context: ExtractionErrorContext,
    /// A copy of the template where the failure happened
    pub template: Value,
    /// What remains of the value that caused the extraction failure
    ///
    /// This value might have been partially extracted already.
    pub value: Value,
}

/// Context where an extraction failure happened
///
/// The operation that failed
#[derive(Debug, Clone, Copy)]
pub enum ExtractionErrorContext {
    /// Failure happened while extracting a value at a static location
    ExtractingStaticValue,
    /// Failure happened while extracting an array of values
    FindingPathToArray,
    /// Failure happened while extracting a value inside of an array
    ExtractingArrayItem,
}

/// Kind of errors that can happen during extraction
#[derive(Debug)]
pub enum ExtractionErrorKind {
    /// An expected path component is missing
    MissingPathComponent {
        /// Index of the missing component in the path
        missing_index: usize,
        /// Path where a component is missing
        path: ValuePath,
    },
    /// An expected path component cannot be found because its container is the wrong type
    WrongPathComponent {
        /// String representation of the wrong component
        wrong_component: String,
        /// Index of the wrong component in the path
        index: usize,
        /// Path where a component has the wrong type
        path: ValuePath,
    },
}

impl ValueTemplate {
    /// Prepare a template for injection or extraction.
    ///
    /// # Parameters
    ///
    /// - `template`: JSON value that acts a template. Its placeholder values will be replaced by actual values during injection,
    ///   and actual values will be recovered from their location during extraction.
    /// - `placeholder_string`: Value that a JSON string should assume to act as a placeholder value that can be injected into or
    ///   extracted from.
    /// - `repeat_string`: Sentinel value that can be placed as the second value in an array to indicate that the first value can be repeated
    ///   any number of times. The first value should contain exactly one placeholder string.
    ///
    /// # Errors
    ///
    /// - Nested repeat string
    /// - repeat string in position 0
    /// - repeat string in another position
    /// - repeat string used in an object or at the top-level rather than as an array item
    pub fn new(
        template: Value,
        placeholder_string: String,
        repeat_string: String,
    ) -> Result<Self, TemplateParsingError> {
        let mut value_path = None;
        let mut array_path = None;
        let mut current_path = Vec::new();
        Self::parse_value(
            &template,
            &placeholder_string,
            &repeat_string,
            &mut value_path,
            &mut Some(&mut array_path),
            &mut current_path,
        )?;
        Ok(Self { template, value_path, array_path })
    }

    /// Whether there is a placeholder that is not repeated.
    ///
    /// - During injection and extraction the static value must be populated if present.
    pub fn has_static_value(&self) -> bool {
        self.value_path.is_some()
    }

    /// Whether there is a placeholder that can be repeated.
    ///
    /// - During injection, any excess values after injecting the static placeholder are injected in the array placeholder,
    /// - During extraction, all repeatable placeholders are extracted from the array.
    pub fn has_array_value(&self) -> bool {
        self.array_path.is_some()
    }

    /// Render a value from the template and context values.
    ///
    /// # Error
    ///
    /// - [`MissingStaticValue`]: if the number of injected values is 0 and there is a static placeholder.
    pub fn inject(
        &self,
        values: impl IntoIterator<Item = Value>,
    ) -> Result<Value, MissingStaticValue> {
        let mut rendered = self.template.clone();
        let mut values = values.into_iter();

        if let Some(injection_path) = &self.value_path {
            let Some(injected_value) = values.next() else { return Err(MissingStaticValue) };
            let mut current_value = &mut rendered;
            for injection_component in injection_path {
                current_value = match injection_component {
                    PathComponent::MapKey(key) => current_value.get_mut(key).unwrap(),
                    PathComponent::ArrayIndex(index) => current_value.get_mut(index).unwrap(),
                }
            }
            *current_value = injected_value;
        }

        if let Some(ArrayPath { repeated_value, path_to_array, value_path_in_array }) =
            &self.array_path
        {
            // 1. build the array or repeated values
            let mut array = Vec::new();
            for injected_value in values {
                let mut repeated_value = repeated_value.clone();
                let mut current_value = &mut repeated_value;
                for injection_component in value_path_in_array {
                    current_value = match injection_component {
                        PathComponent::MapKey(key) => current_value.get_mut(key).unwrap(),
                        PathComponent::ArrayIndex(index) => current_value.get_mut(index).unwrap(),
                    }
                }
                *current_value = injected_value;
                array.push(repeated_value);
            }
            // 2. inject at the injection point in the rendered value
            let mut current_value = &mut rendered;
            for injection_component in path_to_array {
                current_value = match injection_component {
                    PathComponent::MapKey(key) => current_value.get_mut(key).unwrap(),
                    PathComponent::ArrayIndex(index) => current_value.get_mut(index).unwrap(),
                }
            }
            *current_value = Value::Array(array);
        }

        Ok(rendered)
    }

    /// Extract sub values from the template and a value.
    ///
    /// # Errors
    ///
    /// - if a static placeholder is missing.
    /// - if there is no value corresponding to an array placeholder
    /// - if the value corresponding to an array placeholder is not an array
    pub fn extract(&self, mut value: Value) -> Result<ExtractedValues, ExtractionError> {
        let mut static_value = None;

        for extraction_path in self.value_path.iter() {
            let extracted_value =
                extract_value(extraction_path, &mut value).with_context(|kind| {
                    ExtractionError {
                        kind,
                        context: ExtractionErrorContext::ExtractingStaticValue,
                        template: self.template.clone(),
                        value: value.clone(),
                    }
                })?;
            static_value = Some(extracted_value);
        }

        let mut array_values = None;

        if let Some(ArrayPath { repeated_value: _, path_to_array, value_path_in_array }) =
            &self.array_path
        {
            // get the array
            let array =
                extract_value(path_to_array, &mut value).with_context(|kind| ExtractionError {
                    kind,
                    context: ExtractionErrorContext::FindingPathToArray,
                    template: self.template.clone(),
                    value: value.clone(),
                })?;
            let array = match array {
                Value::Array(array) => array,
                not_array => {
                    let mut path = path_to_array.clone();
                    path.push(PathComponent::ArrayIndex(0));
                    return Err(ExtractionError {
                        kind: ExtractionErrorKind::WrongPathComponent {
                            wrong_component: format_value(&not_array),
                            index: path_to_array.len(),
                            path,
                        },
                        context: ExtractionErrorContext::FindingPathToArray,
                        template: self.template.clone(),
                        value: value.clone(),
                    });
                }
            };
            let mut extracted_values = Vec::with_capacity(array.len());

            for mut item in array {
                let extracted_value =
                    extract_value(value_path_in_array, &mut item).with_context(|kind| {
                        ExtractionError {
                            kind,
                            context: ExtractionErrorContext::ExtractingArrayItem,
                            template: self.template.clone(),
                            value: value.clone(),
                        }
                    })?;
                extracted_values.push(extracted_value);
            }

            array_values = Some(extracted_values);
        }

        Ok(ExtractedValues { static_value, array_values })
    }

    fn parse_array(
        array: &[Value],
        placeholder_string: &str,
        repeat_string: &str,
        value_path: &mut Option<ValuePath>,
        mut array_path: &mut Option<&mut Option<ArrayPath>>,
        current_path: &mut ValuePath,
    ) -> Result<(), TemplateParsingError> {
        // two modes for parsing array.
        match array {
            // 1. array contains a repeat string in second position
            [first, second, rest @ ..] if second == repeat_string => {
                let Some(array_path) = &mut array_path else {
                    return Err(TemplateParsingError::NestedRepeatString(current_path.clone()));
                };
                if let Some(array_path) = array_path {
                    return Err(TemplateParsingError::MultipleRepeatString(
                        current_path.clone(),
                        array_path.path_to_array.clone(),
                    ));
                }
                if first == repeat_string {
                    return Err(TemplateParsingError::BadIndexForRepeatString(
                        current_path.clone(),
                        0,
                    ));
                }
                if let Some(position) = rest.iter().position(|value| value == repeat_string) {
                    let position = position + 2;
                    return Err(TemplateParsingError::BadIndexForRepeatString(
                        current_path.clone(),
                        position,
                    ));
                }

                let value_path_in_array = {
                    let mut value_path = None;
                    let mut current_path = Vec::new();

                    Self::parse_value(
                        first,
                        placeholder_string,
                        repeat_string,
                        &mut value_path,
                        &mut None,
                        &mut current_path,
                    )?;

                    value_path.ok_or_else(|| {
                        TemplateParsingError::MissingPlaceholderInRepeatedValue(
                            current_path.clone(),
                        )
                    })?
                };
                **array_path = Some(ArrayPath {
                    repeated_value: first.to_owned(),
                    path_to_array: current_path.clone(),
                    value_path_in_array,
                });
            }
            // 2. array does not contain a repeat string
            array => {
                if let Some(position) = array.iter().position(|value| value == repeat_string) {
                    return Err(TemplateParsingError::BadIndexForRepeatString(
                        current_path.clone(),
                        position,
                    ));
                }
                for (index, value) in array.iter().enumerate() {
                    current_path.push(PathComponent::ArrayIndex(index));
                    Self::parse_value(
                        value,
                        placeholder_string,
                        repeat_string,
                        value_path,
                        array_path,
                        current_path,
                    )?;
                    current_path.pop();
                }
            }
        }
        Ok(())
    }

    fn parse_object(
        object: &Map<String, Value>,
        placeholder_string: &str,
        repeat_string: &str,
        value_path: &mut Option<ValuePath>,
        array_path: &mut Option<&mut Option<ArrayPath>>,
        current_path: &mut ValuePath,
    ) -> Result<(), TemplateParsingError> {
        for (key, value) in object.iter() {
            current_path.push(PathComponent::MapKey(key.to_owned()));
            Self::parse_value(
                value,
                placeholder_string,
                repeat_string,
                value_path,
                array_path,
                current_path,
            )?;
            current_path.pop();
        }
        Ok(())
    }

    fn parse_value(
        value: &Value,
        placeholder_string: &str,
        repeat_string: &str,
        value_path: &mut Option<ValuePath>,
        array_path: &mut Option<&mut Option<ArrayPath>>,
        current_path: &mut ValuePath,
    ) -> Result<(), TemplateParsingError> {
        match value {
            Value::String(str) => {
                if placeholder_string == str {
                    if let Some(value_path) = value_path {
                        return Err(TemplateParsingError::MultiplePlaceholderString(
                            current_path.clone(),
                            value_path.clone(),
                        ));
                    }

                    *value_path = Some(current_path.clone());
                }
                if repeat_string == str {
                    return Err(TemplateParsingError::RepeatStringNotInArray(current_path.clone()));
                }
            }
            Value::Null | Value::Bool(_) | Value::Number(_) => {}
            Value::Array(array) => Self::parse_array(
                array,
                placeholder_string,
                repeat_string,
                value_path,
                array_path,
                current_path,
            )?,
            Value::Object(object) => Self::parse_object(
                object,
                placeholder_string,
                repeat_string,
                value_path,
                array_path,
                current_path,
            )?,
        }
        Ok(())
    }
}

fn format_value(value: &Value) -> String {
    match value {
        Value::Array(array) => format!("array of size {}", array.len()),
        Value::Object(object) => {
            format!("object with {} fields", object.len())
        }
        value => value.to_string(),
    }
}

fn extract_value(
    extraction_path: &Vec<PathComponent>,
    initial_value: &mut Value,
) -> Result<Value, ExtractionErrorKind> {
    let mut current_value = initial_value;
    for (path_index, extraction_component) in extraction_path.iter().enumerate() {
        current_value = {
            match extraction_component {
                PathComponent::MapKey(key) => {
                    if !current_value.is_object() {
                        return Err(ExtractionErrorKind::WrongPathComponent {
                            wrong_component: format_value(current_value),
                            index: path_index,
                            path: extraction_path.clone(),
                        });
                    }
                    match current_value.get_mut(key) {
                        Some(value) => value,
                        None => {
                            return Err(ExtractionErrorKind::MissingPathComponent {
                                missing_index: path_index,
                                path: extraction_path.clone(),
                            });
                        }
                    }
                }
                PathComponent::ArrayIndex(index) => {
                    if !current_value.is_array() {
                        return Err(ExtractionErrorKind::WrongPathComponent {
                            wrong_component: match current_value {
                                Value::Array(array) => format!("array of size {}", array.len()),
                                Value::Object(object) => {
                                    format!("object with {} fields", object.len())
                                }
                                current_value => current_value.to_string(),
                            },
                            index: path_index,
                            path: extraction_path.clone(),
                        });
                    }
                    match current_value.get_mut(index) {
                        Some(value) => value,
                        None => {
                            return Err(ExtractionErrorKind::MissingPathComponent {
                                missing_index: path_index,
                                path: extraction_path.clone(),
                            });
                        }
                    }
                }
            }
        };
    }
    let extracted_value = current_value.take();
    Ok(extracted_value)
}

trait ExtractionResultErrorContext<T> {
    fn with_context<F>(self, f: F) -> Result<T, ExtractionError>
    where
        F: FnOnce(ExtractionErrorKind) -> ExtractionError;
}

impl<T> ExtractionResultErrorContext<T> for Result<T, ExtractionErrorKind> {
    fn with_context<F>(self, f: F) -> Result<T, ExtractionError>
    where
        F: FnOnce(ExtractionErrorKind) -> ExtractionError,
    {
        match self {
            Ok(t) => Ok(t),
            Err(kind) => Err(f(kind)),
        }
    }
}

#[cfg(test)]
mod test {
    use serde_json::{json, Value};

    use super::{PathComponent, TemplateParsingError, ValueTemplate};

    fn new_template(template: Value) -> Result<ValueTemplate, TemplateParsingError> {
        ValueTemplate::new(template, "{{text}}".into(), "{{..}}".into())
    }

    #[test]
    fn empty_template() {
        let template = json!({
            "toto": "no template at all",
            "titi": ["this", "will", "still", "work"],
            "tutu": null
        });

        let empty = new_template(template.clone()).unwrap();

        assert_eq!(empty.has_static_value(), false);
        assert_eq!(empty.has_array_value(), false);

        assert_eq!(empty.inject(vec![]).unwrap(), template);
        assert_eq!(empty.inject(vec!["test".into()]).unwrap(), template);
    }

    #[test]
    fn static_template() {
        let template = json!({
            "toto": "text",
            "titi": ["this", "will", "still", "{{text}}"],
            "tutu": null
        });

        let basic = new_template(template.clone()).unwrap();

        assert_eq!(basic.has_static_value(), true);
        assert_eq!(basic.has_array_value(), false);

        assert_eq!(
            basic.inject(vec!["work".into(), Value::Null, "test".into()]).unwrap(),
            json!({
                "toto": "text",
                "titi": ["this", "will", "still", "work"],
                "tutu": null
            })
        );
    }

    #[test]
    fn too_many_placeholders() {
        let template = json!({
            "toto": "{{text}}",
            "titi": ["this", "will", "still", "{{text}}"],
            "tutu": "text"
        });

        match new_template(template.clone()) {
            Err(TemplateParsingError::MultiplePlaceholderString(left, right)) => {
                let (min, max) = if left <= right { (left, right) } else { (right, left) };

                assert_eq!(
                    min,
                    vec![PathComponent::MapKey("titi".into()), PathComponent::ArrayIndex(3)]
                );

                assert_eq!(max, vec![PathComponent::MapKey("toto".into())])
            }
            _ => panic!("should error"),
        }
    }

    #[test]
    fn dynamic_template() {
        let template = json!({
            "toto": "text",
            "titi": [{
                "type": "text",
                "data": "{{text}}"
            }, "{{..}}"],
            "tutu": null
        });

        let basic = new_template(template.clone()).unwrap();

        assert_eq!(basic.has_static_value(), false);
        assert_eq!(basic.has_array_value(), true);

        let injected_values = vec![
            "work".into(),
            Value::Null,
            42.into(),
            "test".into(),
            "tata".into(),
            "titi".into(),
            "tutu".into(),
        ];

        let rendered = basic.inject(injected_values.clone()).unwrap();

        assert_eq!(
            rendered,
            json!({
                "toto": "text",
                "titi": [
                    {
                        "type": "text",
                        "data": "work"
                    },
                    {
                        "type": "text",
                        "data": Value::Null
                    },
                    {
                        "type": "text",
                        "data": 42
                    },
                    {
                        "type": "text",
                        "data": "test"
                    },
                    {
                        "type": "text",
                        "data": "tata"
                    },
                    {
                        "type": "text",
                        "data": "titi"
                    },
                    {
                        "type": "text",
                        "data": "tutu"
                    }
                ],
                "tutu": null
            })
        );

        let extracted_values = basic.extract(rendered).unwrap();
        match extracted_values.array_values {
            Some(array_values) => assert_eq!(array_values, injected_values),
            None => panic!("no array values"),
        }
    }
}
