.PHONY: syntax-check-json-documents
syntax-check-json-documents:
	set -e; \
	for FILE in $$(ls test/json_documents/*.json | sort); \
	do \
		echo syntax check json document $$FILE; \
		echo ; \
		jq . $$FILE; \
	done
