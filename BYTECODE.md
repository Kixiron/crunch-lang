# Bytecode Format

Instruction | 4 Bits / Opcode | 4 Bits | Byte #3 | Byte #4 | Byte #5
----------- | --------------- | ------ | ------- | ------- | -------
Halt        | `0000`          | `0000` | `0x00`  | `0x00`  | `0x00`
Drop        | `0001`          | `0000` | Register Byte 2 | `0x00` | `0x00`
Jump        | `0010`          | `0000` | Index byte 1 | Index byte 2 | Index byte 3 
CondJump    | `0011`          | Register Byte 1 | Register Byte 2 | Index | Index
Print       | `0100`          | Register Byte 1 | Register Byte 2 | Index | Index
LoadInt     | `0101`          | Register Byte 1 | Register Byte 2 | String | String
LoadStr     | `0110`          | Register Byte 1 | Register Byte 2 | String | String
LoadBool    | `0111`          | Register Byte 1 | Register Byte 2 | Bool | `0x00`
DropStr     | `1000`          | String Register | String Register | String Register | String Register 
AddStr      | `1001`          | Register Byte 1 | Register Byte 2 | Register Byte 1 | Register Byte 2
AddInt      | `1010`          | Register Byte 1 | Register Byte 2 | Register Byte 1 | Register Byte 2
SubInt      | `1011`          | Register Byte 1 | Register Byte 2 | Register Byte 1 | Register Byte 2

# String Stack

Bit 1 of strings will either be high or low. If it is low, then the optional second byte will not be included. If the string is longer than 127 chars, then it will be set to high, allowing for strings of a length up to 32,767
