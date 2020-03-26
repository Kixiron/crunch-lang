+++
title = "Strings"
weight = 5
+++

## String Escapes

Each escape sequence in Crunch will be turned into a single [Unicode Scalar Value]

| Escape Sequence |   Hex Value    | Character Represented                                               |
| :-------------: | :------------: | :------------------------------------------------------------------ |
|      `\\`       |     `0x5C`     | Backslash                                                           |
|      `\"`       |     `0x22`     | Double Quote                                                        |
|      `\'`       |     `0x27`     | Single Quote                                                        |
|      `\n`       |     `0x0A`     | Newline                                                             |
|      `\r`       |     `0x0D`     | Carriage Return                                                     |
|      `\t`       |     `0x09`     | Tab                                                                 |
|      `\0`       |     `0x00`     | Null                                                                |
|    `\x{00}`     | 16<sup>2</sup> | `00` as  [hexadecimal]. Hex<sup>1</sup> characters are allowed      |
|    `\o{000}`    | 8<sup>3</sup>  | `000` as [octal]. Characters `0-7` are allowed                      |
|   `\u{0000}`    | 16<sup>4</sup> | `0000` as [hexadecimal]. Hex<sup>1</sup> characters are allowed     |
| `\U{00000000}`  | 16<sup>8</sup> | `00000000` as [hexadecimal]. Hex<sup>1</sup> characters are allowed |
| `\b{00000000}`  | 2<sup>8</sup>  | `00000000` as [binary]. Characters `0-1` are allowed                |

<br />

<sup>1</sup> `0-9`, `a-f`, `A-F`

[Unicode Scalar Value]: https://www.unicode.org/glossary/#unicode_scalar_value
[hexadecimal]: https://en.wikipedia.org/wiki/Hexadecimal
[binary]: https://en.wikipedia.org/wiki/Binary
[octal]: https://en.wikipedia.org/wiki/Octal
