# RISC-V CPU
This repository is for my learning RISC-V CPU.  
<!--
I am implementing CPU using Chisel, referring to [RISC-VとChiselで学ぶ はじめてのCPU自作](https://www.amazon.co.jp/RISC-V%E3%81%A8Chisel%E3%81%A7%E5%AD%A6%E3%81%B6-%E3%81%AF%E3%81%98%E3%82%81%E3%81%A6%E3%81%AECPU%E8%87%AA%E4%BD%9C-%E2%80%95%E2%80%95%E3%82%AA%E3%83%BC%E3%83%97%E3%83%B3%E3%82%BD%E3%83%BC%E3%82%B9%E5%91%BD%E4%BB%A4%E3%82%BB%E3%83%83%E3%83%88%E3%81%AB%E3%82%88%E3%82%8B%E3%82%AB%E3%82%B9%E3%82%BF%E3%83%A0CPU%E5%AE%9F%E8%A3%85%E3%81%B8%E3%81%AE%E7%AC%AC%E4%B8%80%E6%AD%A9-%E2%BB%84%E2%BC%AD-%E6%82%A0%E5%A4%AA%E6%9C%97-ebook/dp/B09CT1T9HT).  
-->

# Feature
```asm
lw rd, offset(rs1)
x[rd] = M[x[rs1] + sext(imm_i)]

sw rs2, offset(rs1)
M[x[rs1] + sext(imm_s)] = x[rs2]

add rd, rs1, rs2
x[rd] = x[rs1] + x[rs2]

sub rd, rs1, rs2
x[rd] = x[rs1] + x[rs2]

addi rs, rs1, imm_i
x[rd] = x[rs1] +  sext(imm_i)

and rd, rs1, rs2
x[rd] = x[rs1] & x[rs2]

or rd, rs1, rs2
x[rd] = x[rs1] | x[rs2]

xor rd, rs1, rs2
x[rd] = x[rs1] ^ x[rs2]

andi rd rs1, imm_i
x[rd] = x[rs1] + sext(imm_i)

ori rd rs1, imm_i
x[rd] = x[rs1] | sext(imm_i)

xori rd rs1, imm_i
x[rd] = x[rs1] ^ sext(imm_i)

```

# Test
sbt "testOnly cpu.HexTest"

# Blue print
<object data="https://docs.google.com/document/d/1CQvRGIKmbReRsouicq_R9BqWo9fp8gGSY-xdJbbxCNE/export?format=pdf" type="application/pdf" width="100%" height="100%">
   <p><b>no display</b>: <a href="https://docs.google.com/document/d/1CQvRGIKmbReRsouicq_R9BqWo9fp8gGSY-xdJbbxCNE/export?format=pdf">download PDF</a>.</p>
</object>
