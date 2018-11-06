# WhatsAppParser

# Install dependencies

stack install hakyll

# Build the website

- cd whatsapp-parser
- stack init
- stack build
- stack exec site build

> **Note:** "stack init" creates stack.yaml file based on whatsapp-parser.cabal

# Watch

- stack exec site watch