# WhatsAppParser

Parser of a "_chat.txt" file exported from a WhatsApp conversation. Have you ever wanted to know some facts from a chat? WhatsAppParser is for you!

## Dependencies

- [Install Cabal](https://www.haskell.org/cabal/index.html#install-upgrade)
- Once you have Cabal, install the Split package by running the following command
  ```
    cabal install split
  ```

## Notes

1. The parser assumes that every message has the following structure:

```
    [%-m/%-d/%y, %-H:%M:%S %p] MESSAGE_AUTHOR: MESSAGE_TEXT
```

This, however, does not seem to be true across all platforms supported by WhatsApp. For the moment, the parser **WILL NOT WORK** if the message structure is different from the one presented above.

2. The parser skips multiline messages for now. 

3. Although acute accents in vowels and the Ñ letter are handled by normalizing each vowel with acute accent to the vowel without the accent and the Ñ letter to N, it is very likely that the program will fail if the input contains characters from other alphabets. Before running the program, clean the input.

4. Some lines in a chat text file are not that important. For example, whenever a message is deleted or a user leaves a group, there is a line representing that action in the chat, but, in general, the analysis shouldn't take such lines into account. This is a regex that I've used for cleaning a chat's text file.

    ```
    ^.*\b(left|changed|deleted|created|new number|added|joined|removed|omitted)\b.*$
    ```

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