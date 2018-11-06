# WhatsAppParser

# Install dependencies

stack install hakyll

# Build the website

- cd whatsapp-parser
- stack init  # creates stack.yaml file based on whatsapp-parser.cabal
- stack build
- stack exec site build

# Watch

- stack exec site watch