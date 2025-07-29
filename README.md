# firefox_bookmark_imager

This program images the content of a root folder in the firefox bookmarks into a own syntax onto plain text. The data is received from the "places.sqlite" database. The output of stdout is line based and predestined for usage in a shell script. It is completely written in Ada.

A perfect example is the automatic downloading of youtube videos (see examples/).

## Current state

The software is untested and can be used on own risk.

## Building

`alr build`

## Usage

`./bin/main`

## Possible future features
- Flag for choosing the output value (e.g. "title" instead of "url", or even lists: "id:title:url")
- Parsing Firefox JSON
- Parsing Firefox HTML
- Parsing for other Browsers
