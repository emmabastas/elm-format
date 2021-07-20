hpc markup elm-format.tix                                                               \
                                                                                        \
    --destdir=hpc-output                                                                \
                                                                                        \
    `# Make sure that hpc can find all the modules and .mix files                   `   \
    --srcdir=avh4-lib                                                                   \
    --srcdir=elm-format-markdown                                                        \
    --srcdir=elm-format-lib                                                             \
    --srcdir=elm-format-test-lib                                                        \
    --srcdir=.                                                                          \
                                                                                        \
    `# elm-format, elm-format-test-lib and elm-format-lib all contain a "Main"      `   \
    `# module. I think hpc opperates under the assumption that this will not        `   \
    `# happen, and it manifests in hpc complaining about hash missmatches akin to:  `   \
    `#                                                                              `   \
    `#   > hpc: hash in tix file for module Main (<hash>) does not match hash in    `   \
    `#   > ./.hpc/Main.mix (<hash>)                                                 `   \
    `#                                                                              `   \
    `# The main module isn't really of interest when looking at coverage anyway,    `   \
    `# so we just ignore the "Main" modules.                                        `   \
    --exclude=Main                                                                      \
                                                                                        \
    $@
