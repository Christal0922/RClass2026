# -*- coding: utf-8 -*-
"""PPT7.pptx 渲染后处理

pandoc pptx 生成物中：
1) 代码块与行内代码都用 <a:latin typeface="Courier"/>，且代码块默认继承正文
   30pt。这里只在【整段都是等宽字体】时才判定为代码块，压到 20pt，
   避免误伤含行内代码的普通段落。
2) 引用块（> ...）被 pandoc 缩进 1 英寸且无项目符号，视觉上像"断掉的列表"；
   这里改回 marL=0 并统一为金色强调色，作为"结论 / 提示"样式。
"""
import sys
from pptx import Presentation
from pptx.util import Pt
from pptx.dml.color import RGBColor

SRC = sys.argv[1] if len(sys.argv) > 1 else "PPT7.pptx"
CODE_PT = float(sys.argv[2]) if len(sys.argv) > 2 else 20.0
QUOTE_PT = float(sys.argv[3]) if len(sys.argv) > 3 else 28.0
MONO = ("Courier", "Consolas", "Courier New")
GOLD = RGBColor(0xFF, 0xD1, 0x66)

def pPr_of(par):
    p = par._p
    return p.find('{http://schemas.openxmlformats.org/drawingml/2006/main}pPr')

prs = Presentation(SRC)
n_code = n_quote = 0
for s in prs.slides:
    for sh in s.shapes:
        if not sh.has_text_frame:
            continue
        for par in sh.text_frame.paragraphs:
            runs = [r for r in par.runs if r.text.strip()]
            if not runs:
                continue
            # --- 代码块：整段都是等宽字体 ---
            if all((r.font.name or "") in MONO for r in runs):
                n_code += 1
                for r in par.runs:
                    r.font.size = Pt(CODE_PT)
                par.line_spacing = 0.98
                par.space_before = Pt(6)
                par.space_after = Pt(0)
                pPr = pPr_of(par)
                if pPr is not None:
                    pPr.set("marL", "0")
                    pPr.set("indent", "0")
                continue
            # --- 引用块：pandoc 显式 marL >= 1in 且无项目符号 ---
            pPr = pPr_of(par)
            if pPr is None:
                continue
            marL = int(pPr.get("marL") or 0)
            has_bullet = pPr.find('{http://schemas.openxmlformats.org/drawingml/2006/main}buChar') is not None
            if marL >= 1000000 and not has_bullet:
                n_quote += 1
                pPr.set("marL", "0")
                pPr.set("indent", "0")
                for r in par.runs:
                    if r.text.strip():
                        r.font.size = Pt(QUOTE_PT)
                        r.font.color.rgb = GOLD

prs.save(SRC)
print(f"code blocks -> {CODE_PT}pt : {n_code} | blockquotes styled : {n_quote} | {SRC}")
