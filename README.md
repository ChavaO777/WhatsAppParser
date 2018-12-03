# WhatsAppParser

Parser of a "_chat.txt" file exported from a WhatsApp conversation. Have you ever wanted to know some facts from a chat? WhatsAppParser is for you!

## Dependencies

- [Install Cabal](https://www.haskell.org/cabal/index.html#install-upgrade)
- Once you have Cabal, install the Split package by running the following command
  ```
    cabal install split
  ```

## Notes

The parser assumes that every message has the following structure:

```
    [%-m/%-d/%y, %-H:%M:%S %p] MESSAGE_AUTHOR: MESSAGE_TEXT
```

This, however, does not seem to be true across all platforms supported by WhatsApp. For the moment, the parser **WILL NOT WORK** if the message structure is different from the one presented above. The parser is also skipping multiline messages for now. In the future, both a flexible timestamp format and multiline messages should be supported.

## Commands for compiling and executing

```
$ stack ghc *.hs
$ ./Main path/to/input/file.txt
```

## Project features

| Data displayed                                                        | Implemented   |
| ----------------------------------------------------------------------|---------------|
| Date of the first message                                             | [x]           |
| Date of the last message                                              | [x]           |
| Chat length in days                                                   | [x]           |
| Total amount of messages                                              | [x]           |
| Amount of messages per chat member                                    | [x]           |
| Average amount of messages per day                                    | [x]           |
| Top 50 most common words                                              | [x]           |
| Hour of the day with the most messages                                | [ ]           |
| Day of the week with the most messages                                | [x]           |
| Top 5 most common emojis                                              | [ ]           |
| Length of the longest message streak                                  | [ ]           |
| Longest period without messages and its start date                    | [ ]           |
| Name of the person who usually starts the conversation                | [ ]           |