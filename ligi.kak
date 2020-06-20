# Detection

hook global BufCreate .*[.](li) %{
    set-option buffer filetype ligi
}

# Highlighters

add-highlighter shared/ligi regions
add-highlighter shared/ligi/code default-region group
add-highlighter shared/ligi/string   region c?"       (?<!\\)(?:\\\\)*" group
add-highlighter shared/ligi/mlstring region %{c?\\\\} $                 fill string
add-highlighter shared/ligi/char     region c?'       (?<!\\)(?:\\\\)*' group

add-highlighter shared/ligi/comment region -- $ fill comment
add-highlighter shared/ligi/doc_comment region \(= $                 fill comment
add-highlighter shared/ligi/code/ regex \.(addr|deref|has|val|ptr|len)\b 0:variable # Since these two can never be a valid field access otherwise

add-highlighter shared/ligi/string/   fill string
add-highlighter shared/ligi/char/     fill string

nop %sh{
    escape='\\(?:[nrt\\'\''"]|x[a-zA-Z0-9]{2}|u\{[a-zA-Z0-9]+\})'
    cat <<KAK
        add-highlighter shared/ligi/string/ regex ${escape} 0:default+b
        add-highlighter shared/ligi/char/ regex ${escape} 0:default+b
        add-highlighter shared/ligi/char/ regex %{'(?:[^\\]|${escape})([^']+)'} 1:Error
KAK
}

add-highlighter shared/ligi/code/ regex \b(using|const|property|field|alias|let|cvar|var|extern|packed|export|pub|noalias|inline|comptime|nakedcc|stdcallcc|atomic|volatile|align|section)\b 0:keyword
add-highlighter shared/ligi/code/ regex \b(block|overload|distinct|struct|enum|union|concept)\b 0:keyword
add-highlighter shared/ligi/code/ regex \b(array|ref|slice|pure|assert|break|return|continue|defer|errdefer|unreachable|async|suspend|await|cancel)\b 0:keyword
add-highlighter shared/ligi/code/ regex \b(caseof|if|elif|finally|else|switch|and|or|xor|not|in)\b 0:keyword
add-highlighter shared/ligi/code/ regex \b(until|while|for|loop|when|is)\b 0:keyword
add-highlighter shared/ligi/code/ regex \b(fn|macro|do|use|test)\b 0:keyword

add-highlighter shared/ligi/code/ regex \b(anyenum|str|bool|f32|f64|f128|void|noreturn|type|error|promise)\b 0:type
add-highlighter shared/ligi/code/ regex \b(u\d+|i\d+|isize|usize)\b|\b(\d+(\.\d+)?)(u\d+|i\d+|isize|usize|f32|f64)\b 1:type 2:variable 4:type
add-highlighter shared/ligi/code/ regex \b(c_short|c_ushort|c_int|c_uint|c_long|c_ulong|c_longlong|c_ulonglong|c_longdouble|c_void)\b 0:type

add-highlighter shared/ligi/code/ regex \b(null|undef|this)\b 0:variable
add-highlighter shared/ligi/code/ regex \b(true|false)\b 0:value
add-highlighter shared/ligi/code/ regex \b[0-9]+(?:.[0-9]+)?(?:[eE][+-]?[0-9]+)?\b 0:value # decimal numeral
add-highlighter shared/ligi/code/ regex \b0x[a-fA-F0-9]+(?:[a-fA-F0-9]+(?:[pP][+-]?[0-9]+)?)? 0:value # hexadecimal numeral
add-highlighter shared/ligi/code/ regex \b0o[0-7]+ 0:value # octal numeral
add-highlighter shared/ligi/code/ regex \b0b[01]+(?:.[01]\+(?:[eE][+-]?[0-9]+)?)?" 0:value # binary numeral

add-highlighter shared/ligi/code/ regex @(addWithOverflow|bitCast|bitreverse|bytesToSlice|clz|compileError|compileLog|ctz|divExact|divFloor|divTrunc|embedFile|enumToInt|fence|inlineCall|memcpy|memset|mulWithOverflow|panic|popCount|rem|setCold|shlExact|shlWithOverflow|shrExact|sizeOf|sliceToBytes|sqrt|subWithOverflow|This|typeName|typeOf|Vector)\b 0:builtin

add-highlighter shared/ligi/code/ regex ((\+|-|\*|<<)%?|/|=|\^|&|\?|\||!|>|<|%|>>)=? 0:operator
add-highlighter shared/ligi/code/ regex -> 0:operator



add-highlighter shared/ligi/code/  regex `(\w|\d)+\b 0:variable

# Commands

define-command -hidden ligi-filter-around-selections %{
    # remove trailing whitspace
    try %{ execute-keys -draft -itersel <a-x> s\h+$<ret> d }
}

define-command -hidden ligi-indent-on-new-line %[
    evaluate-commands -draft -itersel %[
        # copy comment prefix //
        try %{ execute-keys -draft k <a-x> s^\h*\K///?\h*<ret> y gh j P }
        # preserve indent
        try %{ execute-keys -draft ';' K <a-&> }
        # filter previous line
        try %{ execute-keys -draft k :ligi-filter-around-selections<ret> }
        # indent after lines ending with { or (
        try %[ execute-keys -draft k <a-x> <a-k>[{(]\h*$<ret> j <a-gt> ]
    ]
]

define-command -hidden ligi-indent-on-closing-curly-brace %[
    evaluate-commands -draft -itersel %[
        # align to opening brace when the closing brace is the only thing on this line
        try %[ execute-keys -draft <a-h> <a-k>^\h+\}$<ret> h m s\A|.\z<ret> 1<a-&> ]
    ]
]

# Initialization

hook -group ligi-highlight global WinSetOption filetype=ligi %{
    add-highlighter window/ligi ref ligi

    hook -once -always window WinSetOption filetype=.* %{
        remove-highlighter window/ligi
    }
}

hook global WinSetOption filetype=ligi %[
    set-option buffer comment_line '//'
    hook -group ligi-hooks window ModeChange insert:.* ligi-filter-around-selections
    hook -group ligi-indent window InsertChar \n ligi-indent-on-new-line
    hook -group ligi-indent window InsertChar \} ligi-indent-on-closing-curly-brace

    hook -once -always window WinSetOption filetype=.* %{
        remove-hooks window ligi-.+
    }
]
