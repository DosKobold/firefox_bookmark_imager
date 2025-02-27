# firefox_bookmark_imager

This program images the content of a root folder in the firefox bookmarks onto plain text. The data is received from the "places.sqlite" database. The output of stdout is predestined for usage in a shell pipe. It is completely written in Ada.

## Current state

The software is still in development and will get more features.

### TODO:
- Refactor the imaging package to a "class" => Put private variables into a type
- Flag for checking syntax (no pre or post marking elements in strings)
- Flag for choosing the output value (e.g. "title" instead of "url", or even lists: "id:title:url")

## Possible future features
- Parsing Firefox JSON
- Parsing Firefox HTML
- Parsing for other Browsers

## Building

`alr build`