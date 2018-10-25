SHELL=/bin/bash

MAIN = Main.hs
BNFC_DIR = __BNFC__
LANG_NAME = Americano
AMERICANO_DIR = $(LANG_NAME)
GRAMMAR_NAME = Grammar
GRAMMAR_FILE = $(GRAMMAR_NAME).cf
BNFC_SUFFIX = $(GRAMMAR_NAME)
MODULE_PREFIX = $(LANG_NAME).

all: $(MAIN) $(AMERICANO_DIR)
	ghc --make $(MAIN) -o interpreter

dev-clean: dev clean-dev

dev: bnfc Syntax Lexer Parser Printer Skeleton Test
	$(eval TARGET := $(AMERICANO_DIR)/ErrMonad.hs)
	cp $(BNFC_DIR)/ErrM.hs $(TARGET)
	$(call rename_module,ErrM,ErrMonad,$(TARGET))

bnfc:
	mkdir -p $(BNFC_DIR)
	mkdir -p $(AMERICANO_DIR)
	cd $(BNFC_DIR) && bnfc -m ../$(GRAMMAR_FILE) && make
	
Syntax:
	$(call copy_module,Abs,$@,hs)

Lexer:
	$(call copy_module,Lex,$@,hs)
	$(call copy_module,Lex,$@,x)

Parser:
	$(call copy_module,Par,$@,hs)

Printer:
	$(call copy_module,Print,$@,hs)

Skeleton:
	$(call copy_module,Skel,$@,hs)
	sed -i -e 's/trans/denote/g' $(AMERICANO_DIR)/Skeleton.hs

Test:
	$(call copy_module,Test,$@,hs)

clean: clean-dev
	-find . -regextype posix-egrep -regex '.*\.(hi|log|aux|o)$$' -type f -delete
	-rm interpreter

clean-dev:
	-rm -rf $(BNFC_DIR)
	-cd $(AMERICANO_DIR) && rm -f Skeleton.hs Test.hs 

define rename_module
	$(eval OLD_NAME := $(1))
	$(eval NEW_NAME := $(2))
	$(eval TARGET := $(3))
	sed -i -e 's/$(OLD_NAME)/$(MODULE_PREFIX)$(NEW_NAME)/g' $(TARGET)
endef

define rename_imports
	$(eval TARGET := $(1))
	$(call rename_module,Abs$(BNFC_SUFFIX),Syntax,$(TARGET))
	$(call rename_module,Lex$(BNFC_SUFFIX),Lexer,$(TARGET))
	$(call rename_module,Par$(BNFC_SUFFIX),Parser,$(TARGET))
	$(call rename_module,Skel$(BNFC_SUFFIX),Skeleton,$(TARGET))
	$(call rename_module,Print$(BNFC_SUFFIX),Printer,$(TARGET))
	$(call rename_module,ErrM,ErrMonad,$(TARGET))
endef

define copy_module
	$(eval BNFC_PREFIX := $(1))
	$(eval BNFC_MODULE_NAME := $(BNFC_PREFIX)$(BNFC_SUFFIX))
	$(eval TARGET_MODULE_NAME := $(2))
	$(eval EXTENSION := $(3))
	$(eval TARGET := $(AMERICANO_DIR)/$(TARGET_MODULE_NAME).$(EXTENSION))
	cd $(BNFC_DIR) && cp $(BNFC_MODULE_NAME).$(EXTENSION) ../$(TARGET)
	$(call rename_module,$(BNFC_MODULE_NAME),$(TARGET_MODULE_NAME),$(TARGET))
	$(call rename_imports,$(TARGET))
endef
