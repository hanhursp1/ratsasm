import
  std/[strutils, tables],
  bitbuilder

type
  AssemblerException = object of Defect

type
  Register* = range[0..7]
  SubOp* = range[0..7]
  Label* = string
  Literal* = byte
  Flag* = bool  # Unused as of now

  ArgKind* {.pure.} = enum
    Register, SubOp, Label, Literal, Flag
  
  Argument* = object
    case kind*: ArgKind
    of Register:
      reg*: Register
    of SubOp:
      sop*: SubOp
    of Label:
      lab*: Label
    of Literal:
      lit*: Literal
    of Flag:
      flg*: Flag

  AssemblerInfo* = object
    line*: int
    str*: string

  TokenRaw* = object
    op*: string
    args*: seq[Argument]
    info*: AssemblerInfo
  
  TokenKind* {.pure.} = enum
    NULL, LABEL, NOP, ALU, MOV, MOVC, LDR, STR, INC, DEC, JMP, JMPC, FN, FNC,
    JMPEQ, JMPNEQ, JMPLT, JMPGT, JMPEQC, JMPNEQC, JMPLTC, JMPGTC,
    CMP, CMPZ
  
  Token* = object
    kind*: TokenKind
    args*: seq[Argument]
    info*: AssemblerInfo
  
  Assembler* = object
    labelTable*: Table[Label, byte]

template asReg*(this: byte): Argument =
  Argument(
    kind: ArgKind.Register,
    reg: this
  )

template asLabel*(this: string): Argument =
  Argument(
    kind: ArgKind.Label,
    lab: this
  )

template asSubOp*(this: byte): Argument =
  Argument(
    kind: ArgKind.SubOp,
    sop: this
  )

template asLit*(this: byte): Argument =
  Argument(
    kind: ArgKind.Literal,
    lit: this
  )

template asFlag*(this: bool): Argument =
  Argument(
    kind: ArgKind.Flag,
    flg: this
  )

proc getLabels*(this: Assembler): seq[(Label, byte)] =
  for (label, address) in this.labelTable.pairs():
    result.add((label, address))

proc isEmpty*(this: TokenRaw): bool =
  this.op.isEmptyOrWhitespace()

proc setLabel*(this: var Assembler, label: string, val: byte) =
  this.labelTable[label] = val

proc getLabel*(this: Assembler, label: string): Literal =
  this.labelTable[label]

const RET_ARGS = @[
  asReg(6)
]

proc getTokenKind*(op: string): tuple[tokKind: TokenKind, varaint: byte] =
  result = (TokenKind.NULL, 0)
  # echo op
  if op.startsWith('.'):
    result.tokKind = TokenKind.LABEL
  case op
  of "nop":
    result = (TokenKind.NOP, 0x00)
  # ALU Operations (base)
  of "add":
    result = (TokenKind.ALU, 0x00)
  of "sub":
    result = (TokenKind.ALU, 0x01)
  of "shl":
    result = (TokenKind.ALU, 0x02)
  of "shr":
    result = (TokenKind.ALU, 0x03)
  of "not":
    result = (TokenKind.ALU, 0x04)
  of "xor":
    result = (TokenKind.ALU, 0x05)
  of "or":
    result = (TokenKind.ALU, 0x06)
  of "and":
    result = (TokenKind.ALU, 0x07)
  # ALU Operations (set flags)
  of "adds":
    result = (TokenKind.ALU, 0x10)
  of "subs":
    result = (TokenKind.ALU, 0x11)
  of "shls":
    result = (TokenKind.ALU, 0x12)
  of "shrs":
    result = (TokenKind.ALU, 0x13)
  of "nots":
    result = (TokenKind.ALU, 0x14)
  of "xors":
    result = (TokenKind.ALU, 0x15)
  of "ors":
    result = (TokenKind.ALU, 0x16)
  of "ands":
    result = (TokenKind.ALU, 0x17)
  # Register ops
  of "mov":
    result.tokKind = TokenKind.MOV
  of "ldr":
    result.tokKind = TokenKind.LDR
  of "str":
    result.tokKind = TokenKind.STR
  of "inc":
    result.tokKind = TokenKind.INC
  of "dec":
    result.tokKind = TokenKind.DEC
  # Branching ops
  of "jmp":
    result.tokKind = TokenKind.JMP
  of "fn":
    result.tokKind = TokenKind.FN
  of "jmpeq":
    result.tokKind = TokenKind.JMPEQ
  of "jmpneq":
    result.tokKind = TokenKind.JMPNEQ
  of "jmplt":
    result.tokKind = TokenKind.JMPLT
  of "jmpgt":
    result.tokKind = TokenKind.JMPGT
  of "cmp":
    result.tokKind = TokenKind.CMP
  of "cmpz":
    result.tokKind = TokenKind.CMPZ


proc finalizeToken*(tok: TokenRaw): Token =
  var (kind, variant) = tok.op.getTokenKind()
  var args = tok.args
  if kind == TokenKind.ALU:
    # Check if the returned variant is greater than or equal to 0x10, because that means
    # it's an S variant instructions (update status register)
    if (variant and 0x07) == 0x04 and args.len() < 3:
      args.add(asReg(0))
    if variant >= 0x10:
      variant = variant and 0x07
      args.add(asFlag(true))
    else:
      args.add(asFlag(false))
    args = @[asSubOp(variant)] & args

  elif kind == TokenKind.MOV:
    # Check if the second argument is a register or a constand
    if args[1].kind in {ArgKind.Literal, ArgKind.Label}:
      kind = TokenKind.MOVC
  elif kind in {TokenKind.JMP, TokenKind.FN}:
    if args[0].kind in {ArgKind.Literal, ArgKind.Label}:
      kind = TokenKind(kind.ord + 1)
  elif kind in {TokenKind.JMPEQ, TokenKind.JMPNEQ, TokenKind.JMPLT, TokenKind.JMPGT}:
    if args[0].kind in {ArgKind.Literal, ArgKind.Label}:
      kind = TokenKind(kind.ord + 4)
  elif kind in {TokenKind.INC, TokenKind.DEC}:
    if args.len() < 2:
      args.add(asLit(1))
  elif kind == TokenKind.LABEL:
    args = @[asLabel(tok.op)]
  elif kind in {TokenKind.STR, TokenKind.LDR}:
    if args.len() < 3:
      args.add(asLit(0))

  return Token(
    kind: kind,
    args: args,
    info: tok.info
  )
    

proc preAssemble*(this: var Assembler, rawTokenStream: seq[TokenRaw]): seq[Token] =
  result = newSeqOfCap[Token](rawTokenStream.len())
  var i = 0
  for tok in rawTokenStream:
    if tok.op.startsWith('.'):
      this.setLabel(tok.op[0..^1], (i * 2).byte)
      continue
    if tok.op == "ret":
      result.add Token(
        kind: TokenKind.JMP,
        args: RET_ARGS,
        info: tok.info
      )
    else: result.add tok.finalizeToken()
    i.inc

proc replaceLabel(this: Assembler, tok: Token): Token =
  result = tok
  for i in result.args.mitems():
    if i.kind == ArgKind.Label:
      i = Argument(
        kind: ArgKind.Literal,
        lit: this.getLabel(i.lab)
      )

proc assembleToken*(this: Assembler, tok: Token): uint16 =
  let tok = this.replaceLabel(tok)
  uint16 case tok.kind:
  of NOP:
    (0)
  # ALU Operations
  of ALU:
    (
      0b01.shl(14)                        or
      tok.args[0].sop.width(3).shl(11)    or
      tok.args[1].reg.width(3).shl(8)     or
      tok.args[2].reg.width(3).shl(5)     or
      tok.args[3].reg.width(3).shl(2)     or
      tok.args[4].flg.ord.shl(1)       
    )
  # Register operations
  of MOV:
    (
      0b10.shl(14)                        or
      0b100.shl(11)                       or
      tok.args[0].reg.width(3).shl(8)     or
      tok.args[1].reg.width(3).shl(5)
    )
  of MOVC:
    (
      0b10.shl(14)                        or
      0b101.shl(11)                       or
      tok.args[0].reg.width(3).shl(8)     or
      tok.args[1].lit.width(8).int
    )
  of LDR:
    (
      0b10.shl(14)                        or
      0b111.shl(11)                       or
      tok.args[0].reg.width(3).shl(8)     or
      tok.args[1].reg.width(3).shl(5)     or
      tok.args[2].lit.width(5).int
    )
  of STR:
    (
      0b10.shl(14)                        or
      0b110.shl(11)                       or
      tok.args[0].reg.width(3).shl(8)     or
      tok.args[1].reg.width(3).shl(5)     or
      tok.args[2].lit.width(5).int
    )
  of INC:
    (
      0b10.shl(14)                        or
      0b000.shl(11)                       or
      tok.args[0].reg.width(3).shl(8)     or
      tok.args[1].lit.width(8).int
    )
  of DEC:
    (
      0b10.shl(14)                        or
      0b001.shl(11)                       or
      tok.args[0].reg.width(3).shl(8)     or
      tok.args[1].lit.width(8).int
    )
  # Branching operations
  of JMP:
    (
      0b11.shl(14)                        or
      0b101.shl(11)                       or
      tok.args[0].reg.width(3).shl(8)
    )
  of JMPC:
    (
      0b11.shl(14)                        or
      0b100.shl(11)                       or
      tok.args[0].lit.width(8).int
    )
  of FN:
    (
      0b11.shl(14)                        or
      0b111.shl(11)                       or
      tok.args[0].reg.width(3).shl(8)
    )
  of FNC:
    (
      0b11.shl(14)                        or
      0b110.shl(11)                       or
      tok.args[0].lit.width(8).int
    )
  of JMPEQ:
    (
      0b11.shl(14)                        or
      0b010.shl(11)                       or
      0b000.shl(8)                        or
      tok.args[0].reg.width(3).shl(5)
    )
  of JMPNEQ:
    (
      0b11.shl(14)                        or
      0b010.shl(11)                       or
      0b001.shl(8)                        or
      tok.args[0].reg.width(3).shl(5)
    )
  of JMPLT:
    (
      0b11.shl(14)                        or
      0b010.shl(11)                       or
      0b011.shl(8)                        or
      tok.args[0].reg.width(3).shl(5)
    )
  of JMPGT:
    (
      0b11.shl(14)                        or
      0b010.shl(11)                       or
      0b010.shl(8)                        or
      tok.args[0].reg.width(3).shl(5)
    )
  of JMPEQC:
    (
      0b11.shl(14)                        or
      0b010.shl(11)                       or
      0b100.shl(8)                        or
      tok.args[0].lit.width(8).int
    )
  of JMPNEQC:
    (
      0b11.shl(14)                        or
      0b010.shl(11)                       or
      0b101.shl(8)                        or
      tok.args[0].lit.width(8).int
    )
  of JMPLTC:
    (
      0b11.shl(14)                        or
      0b010.shl(11)                       or
      0b111.shl(8)                        or
      tok.args[0].lit.width(8).int
    )
  of JMPGTC:
    (
      0b11.shl(14)                        or
      0b010.shl(11)                       or
      0b110.shl(8)                        or
      tok.args[0].lit.width(8).int
    )
  of CMP:
    (
      0b11.shl(14)                        or
      0b001.shl(11)                       or
      0b000.shl(8)                        or
      tok.args[0].reg.width(3).shl(5)     or
      tok.args[1].reg.width(3).shl(2)     or
      0b10
    )
  of CMPZ:
    (
      0b11.shl(14)                        or
      0b001.shl(11)                       or
      0b001.shl(8)                        or
      tok.args[0].reg.width(3).shl(5)     or
      0b10
    )
  else:
    0

proc assemble*(this: Assembler, tokenStream: seq[Token]): array[128, uint16] =
  for i in 0..<tokenStream.len():
    # try:
    #   result[i] = this.assembleToken(tokenStream[i])
    # except Exception as e:
    #   raise newException(AssemblerException, "Error on line " & $tokenStream[i].info.line & ".\n" & $e.name & "\n" & e.msg)
    result[i] = this.assembleToken(tokenStream[i])
