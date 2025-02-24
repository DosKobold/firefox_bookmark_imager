# firefox_bookmark_imager

This program images the content of a root folder in the firefox bookmarks onto plain text. The data is received from the "places.sqlite" database. The output of stdout is predestined for usage in a shell pipe. It is completely written in Ada.

## Current state

The software is still in development and will get more features.  \
\
TODO:
- Flag for disallowing doubles in the same folder (folders and links) -> panic
- Flag for recursion depth
- Flag for checking syntax (no "\n" in names or "./" at the beginning of objects)
- Flag for choosing the output value (e.g. "title" instead of "url", or even lists: "id:title:url")
- Flag for pre marking elements, especially folders (e.g. "*" instead of "./")
- Flag for post marking elements, especially folders (e.g. "}" instead of "\n")
- Implement the conversion with a JSON file as input (and HTML?)

## Building

`alr build`