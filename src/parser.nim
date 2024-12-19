import
  std/[strutils, options, sequtils],
  tokens

proc parseArg(arg: string): Argument =
  let arg = arg.toLowerAscii()
  if arg.startsWith('r'):
    result = Argument(
      kind: ArgKind.Register,
      reg: arg[1..^1].parseInt().byte
    )
  elif arg.startsWith('.'):
    result = Argument(
      kind: ArgKind.Label,
      lab: arg
    )
  else:
    let num = if arg.startsWith("0x"):
        arg[2..^1].parseHexInt()
      elif arg.startsWith("0b"):
        arg[2..^1].parseBinInt()
      elif arg.startsWith("0o"):
        arg[2..^1].parseOctInt()
      else:
        arg.parseInt()
    result = Argument(
      kind: ArgKind.Literal,
      lit: num.byte
    )

proc parseOpLine*(line: string): TokenRaw =
  # Keeps track of placement
  var i = 0
  # Tokenize the line
  for tok in line.tokenize({' ', ','}):
    if tok.token == ";": break
    if tok.isSep: continue
    if i == 0:
      result.op = tok.token.toLowerAscii()
    else:
      result.args.add(parseArg(tok.token.toLowerAscii()))
    # Increment i
    i.inc

proc parseAsm*(code: string): (array[128, uint16], seq[Token]) =
  var rawTokens: seq[TokenRaw]
  var i = 1
  for line in code.splitLines():
    defer: i.inc
    var p = line.parseOpLine()
    p.info.line = i
    p.info.str = line
    if p.isEmpty(): continue
    rawTokens.add(p)
  
  var assembler: Assembler
  let tokens = assembler.preAssemble(rawTokens)

  result[0] = assembler.assemble(tokens)
  result[1] = tokens

  # echo assembler.getLabels()