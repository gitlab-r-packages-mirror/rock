Hey there,


0.7.1: Implemented a work-around for when the recursion in rbind_df_list() throws a C stack error.

0.7.0: Changed the regex for utterance splits from "([\\?\\!]+\\s?|…\\s?|[[:alnum:]'\"]\\s*\\.(?!\\.\\.)\\s?)" to "([\\?\\!]+\\s?|…\\s?|[[:alnum:]')\"]\\s*\\.(?!\\.\\.)\\s?)"

