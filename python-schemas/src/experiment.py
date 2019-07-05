from enum import Enum

from jsonschema.validators import extend
import jsonschema


schema = obj(
    number_of_iterations=integer("The number of times to run the algorithm"),
    input_data=data_frame("The raw data as pulled from adwords"),
)


def primitive(model, description):
    if description is None:
        return {"type": model}
    return {"type": model, "description": description}


def string(description=None):
    return primitive("string", description)


def integer(description=None):
    return primitive("integer", description)


def number(description=None):
    return primitive("number", description)


def boolean(description=None):
    return primitive("boolean", description)


def null(description=None):
    return primitive("null", description)


def with_format(fmt, description=None):
    schema = string(description)
    schema["format"] = fmt
    return schema


def date(description=None):
    return with_format("date", description)


def email(description=None):
    return with_format("email", description)


def host(description=None):
    return with_format("hostname", description)


def array(schema):
    return {"type": "array", "items": schema}


def obj(properties, required=True, optional=None):
    optional = optional or []
    if required is True:
        required = [k for k in properties.keys() if k not in optional]
    elif required is False:
        required = []
    return {"type": "object", "properties": properties, "required": required}


def enumeration(values, description=None):
    if isinstance(values, list):
        schema = string(description)
        schema["enum"] = values
        return schema
    if issubclass(values, Enum):
        return enumeration([e.name for e in values], description)
    raise ValueError("Cannot create enumeration schema from {}".format(values))


def any(*schemas):
    return {"anyOf": list(schemas)}


_TYPE_CHECKER = jsonschema.Draft7Validator.TYPE_CHECKER

try:
    import pandas
except ImportError:
    pass
else:

    def data_frame(description):
        return primitive("data_frame", description)

    def is_data_frame(_checker, value):
        return isinstance(value, pandas.DataFrame)

    _TYPE_CHECKER = _TYPE_CHECKER.redefine("data_frame", is_data_frame)


CustomValidator = extend(jsonschema.Draft7Validator, type_checker=_TYPE_CHECKER)


def validate(schema, value, cls=CustomValidator, **kwargs):
    cls(schema, format_checker=jsonschema.draft7_format_checker).validate(
        value, **kwargs
    )


schema = {"type": "data_frame"}

value = pandas.DataFrame({"x": [1]})


def test():
    try:
        return validate(schema, value)
    except jsonschema.ValidationError as exc:
        return exc
