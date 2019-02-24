
# Naming conventions
* snake_case: The name uses only lower-case letters, with words separated by underscores
* PascalCased: The name starts with a capital letter, and has a capital letter for each new word, with no underscores
* camelCased: Like PascalCase, but with a lower-case first letter
* ALL_CAPITALS: All capital letters, with words separated by underscores


Item      |   Naming convention    |    Example
----------|------------------------|-----------
Modules   | snake_case             | elevator_interface.erl
Variables | PascalCased            | ButtonType
Functions | snake_case             | set_order_button_light()
Arguments | PascalCased            | set_order_button_light(Floor)
Macros    | ALL_CAPITALS           | RECV_PORT

# Formatting
## Conditional statements
### If-statements
```erlang
if
  Num1 > Num2 ->
    % Do something;
  Num1 < Num2 ->
    % Do something else;
  true ->
    % else-statement
end.
```
