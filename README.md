# 6502 Emulator

## Resources used
- 27c3: Reverse Engineering the MOS 6502 CPU (en): <br>
	https://www.youtube.com/watch?v=fWqBmmPQP40&t=34s

- NES Emulator Part #2: The CPU (6502 Implementation): <br>
	https://www.youtube.com/watch?v=8XmxKPJDGU0&t=625s

- Interpreting Instructions in C: <br>
	https://www.youtube.com/watch?v=ZJy2evCLAsM

- Emulator 101 - 6502 Addressing Modes: <br>
	http://www.emulator101.com/6502-addressing-modes.html

- “Hello, world” from scratch on a 6502 — Part 1: <br>
	https://www.youtube.com/watch?v=LnzuMJLZRdU


### Clock algorithm

1. Read byte @ pc
2. Decode the byte in order to get info about the instruction
3. Read 0,1 or 2 more bytes, depending on the decoded byte
4. Execute the instruction
5. Wait, cycling until the instruction is complete
