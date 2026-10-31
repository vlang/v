# Code generation

Run `v generate model User name:string age:int` to generate an ORM model.
Model names use CamelCase; field names use lowercase letters, digits and underscores,
and cannot begin with a digit. Invalid fields are rejected before an output file is written.
