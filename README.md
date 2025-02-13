[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/3vU1zTLs)
# JSON Toolkit

The purpose of this assignment is to deepen your understanding of input/output (IO) operations and type definitions in the functional programming language, Haskell. This assignment will provide you with practical experience in compiling Haskell code, structuring Haskell programs through modules, defining new types, and manipulating files.

## Topic

The topic of the assignment is [JavaScript Object Notation (JSON)](https://www.json.org/json-en.html), a widely used, lightweight data interchange format that is easy for humans to read and write and easy for programs to parse and generate.

JSON is built on two structures:

- a collection of key/value pairs, and
- an ordered list of values.

A JSON value can be one of the following types:

1. **String**: A sequence of characters enclosed in double quotes.
2. **Number**: An integer or floating-point number.
3. **Object**: An unordered collection of key/value pairs enclosed in curly braces `{}`.
4. **Array**: An ordered list of values enclosed in square brackets `[]`.
5. **Boolean**: A value of either `true` or `false`.
6. **Null**: A null value represented by `null`.

Example:

```json
{
  "string": "Hello, World!",
  "number": 42,
  "object": { "key": "value" },
  "array": [1, 2, 3],
  "boolean": true,
  "null": null
}
```

There are numerous tools and libraries available in various programming languages to manipulate and process JSON files. These tools provide features to read, write, and manipulate JSON data efficiently, handling the parsing and formatting of the data.

## Task

The task of the assignment involves creating a command-line utility named `jsonkit` using Haskell. It takes JSON files as a command-line argument and parse them into an appropriate structured representation. This representation will then be used to carry out a variety of operations.

The operations that the utility should support are as follows:

1. __Pretty Print JSON__: This operations formats the JSON data in a way that is easy for humans to read. This typically involves adding indentation and line breaks to make the structure of the JSON data clear.
2. __Minify JSON__: This operation removes all unnecessary whitespace (such as spaces, tabs, and newlines) from the JSON data to reduce its size.
3. __Extract JSON Path__: This operation extracts specific values from a JSON file using a [JSON path expression](https://docs.oracle.com/cd/E60058_01/PDF/8.0.8.x/8.0.8.0.0/PMF_HTML/JsonPath_Expressions.htm).
    - A JSON path expression is a way to specify a location within a JSON document to retrieve specific values.
    - The minimum operators of JSON path expressions that should be supported are:
      - `$`: The root element.
      - `*`: Matches all elements.
      - `.<name>`: Accesses a child element by name.
      - `[<number>]`: Accesses an element in an array by its index.
4. __Merge JSON Files__: This operation combines multiple JSON files into one, provided that they have compatible structures.
5. __Sort JSON Keys__: This operation arranges the keys of JSON objects in a specific order, typically in alphabetical order.

The parsing of JSON needs to be implemented by hand and should not use a third-party library for that. Parsing errors need to be handled and reported appropriately in a comprehensive manner. Operations on JSON files can be performed in-place or create new files according to your choice.

Finally, the utility should also be capable of printing a usage message. This message should clearly describe how to use the JSON command-line utility, providing users with guidance on how to perform each of the supported operations:

```
% ./jsonkit
Usage: jsonkit <operation>
Operations:
  -pretty  <file>                     | Format a JSON file in a human-readable way.
  -minify  <file>                     | Remove all unnecessary whitespace from a JSON file.
  -extract <file> <path>              | Extract a specific value from a JSON file using a path expression.
  -merge   <file> <file>...           | Merge two or more JSON files into one.
  -sort    <file>                     | Sort the keys in a JSON object alphabetically.
```

## How-to

For this task, you have the flexibility to design and implement the JSON toolkit in any way you see fit. You are permitted to leverage the Haskell standard library to meet any requirements you may encounter. However, it's important to remember that you should only use language constructs and features that have been introduced or discussed during our classes. Ultimately, you must understand any functionality you use, both in terms of its programmatic usage and its internal workings. If any part of your code seems to be beyond the scope of what has been covered in class, be prepared to explain that section in detail.

## Grading

The grading of the assignment will be based on the following criteria:

* __Correctness__: The code should compile without errors and perform the task as per the assignment's requirements. It should handle all edge cases and should not crash under any circumstances.
* __Efficiency__: The code should be optimized to run as efficiently as possible. This includes minimizing the time and space complexity of the code. It should not contain unnecessary computation or redundant code.
* __Readability__: The code should be easy to read and understand. This includes proper indentation, the use of meaningful variable and function names, and the inclusion of comments to explain complex parts of the code.
* __Modularity__: The code should be well-structured and divided into functions and modules, each performing a single task. This makes the code easier to read, understand, and maintain.
* __Style__: The code should demonstrate an idiomatic functional programming style, employing suitable programming constructs to ensure the code is concise, modular, and easy to reason about.

Each criterion contributes a maximum of one point to the overall grade. The assignment grade is then computed using the following formula:

<p align="center">
$G = 1 + C_x + E_x + R_x + M_x + S_x$
</p>

where $C_x$, $E_x$, $R_x$, $M_x$, and $S_x$ represent the evaluation of your code based on each respective criterion.

## Deliverable

The deliverable for this assignment is a Haskell program, with an entry point named `jsonkit.hs`. You may structure the program across as many files as necessary. Each file should include a header that identifies the author(s) of the code. The assignment can be completed individually or in pairs. However, it is crucial to ensure that the work submitted is original and not plagiarized. Copying code from the internet or using solutions from other students is strictly prohibited and will be considered a breach of academic integrity.

## Deadline

The deadline for the assignment is **Wednesday, November 20, 2024, at 23:55**. Once this deadline passes, the repository will be locked for grading purposes, and late submissions will not be accepted under any circumstances.
