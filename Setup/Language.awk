BEGIN { 
  printf("\n", $0)
  printf("STRINGTABLE\n", $0)
  printf("BEGIN\n", $0)
}

/^Type/ { gsub("^.*$", "", $0) }
/^Name/ { gsub("^.*$", "", $0) }
/^Author/ { gsub("^.*$", "", $0) }
/^Mail/ { gsub("^.*$", "", $0) }
/^CodePage/ { gsub("^.*$", "", $0) }
/^LanguageId/ { gsub("^.*$", "", $0) }
/^ActiveQueryBuilderLanguage/ { gsub("^.*$", "", $0) }
/^#/ { gsub("^#.*$", "", $0) }
/^;/ { gsub("^;.*$", "", $0) }

/=/ {
  sub("^", "000", $0)
  sub(".... *=", "1&", $0)
  sub("^[^1]*1", "  1", $0)
  gsub("\"", "\"\"", $0)
  sub(" *= *", " \"", $0)
  sub("$", "\"", $0)
  printf("%s\n", $0)
}

END { 
  printf("END\n", $0)
}
