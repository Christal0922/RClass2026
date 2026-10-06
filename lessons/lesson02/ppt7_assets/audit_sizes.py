# -*- coding: utf-8 -*-
"""字号审计：按 run -> 段落 -> 形状lstStyle -> 版式占位符 lstStyle -> 母版 txStyles
逐级解析实际生效字号，列出低于阈值的文字（代码段单独标记）。"""
import sys
from pptx import Presentation
from pptx.oxml.ns import qn

SRC = sys.argv[1] if len(sys.argv) > 1 else "PPT7.pptx"
MIN = float(sys.argv[2]) if len(sys.argv) > 2 else 28.0
MONO = ("Courier", "Consolas", "Courier New")

prs = Presentation(SRC)
master = prs.slide_masters[0]


def lvl_sz_from_lstStyle(lstStyle, lvl):
    if lstStyle is None:
        return None
    pp = lstStyle.find(qn("a:lvl%dpPr" % lvl))
    if pp is None:
        return None
    rpr = pp.find(qn("a:defRPr"))
    if rpr is None or rpr.get("sz") is None:
        return None
    return int(rpr.get("sz")) / 100.0


def master_sz(is_title, lvl):
    tx = master.element.find(qn("p:txStyles"))
    if tx is None:
        return None
    if is_title:
        st = tx.find(qn("p:titleStyle"))
    else:
        st = tx.find(qn("p:bodyStyle"))
    if st is None:
        return None
    pp = st.find(qn("a:lvl%dpPr" % lvl))
    if pp is None:
        return None
    rpr = pp.find(qn("a:defRPr"))
    if rpr is None or rpr.get("sz") is None:
        return None
    return int(rpr.get("sz")) / 100.0


def layout_ph(layout, idx, phtype):
    for ph in layout.placeholders:
        pf = ph.placeholder_format
        if idx is not None and pf.idx == idx:
            return ph
    for ph in layout.placeholders:
        if phtype and ph.placeholder_format.type is not None and str(ph.placeholder_format.type).startswith(phtype):
            return ph
    return None


bad = []
for si, s in enumerate(prs.slides, 1):
    layout = s.slide_layout
    for sh in s.shapes:
        if not sh.has_text_frame or not sh.text_frame.text.strip():
            continue
        ph = None
        try:
            ph = sh.placeholder_format
        except Exception:
            ph = None
        idx = None
        phtype = None
        if ph is not None:
            # 幻灯片形状上的 ph：仅有 type= title/ctrTitle 等
            if ph.type is not None:
                phtype = "TITLE" if str(ph.type).startswith(("TITLE", "CENTER_TITLE")) else None
        # 形状自身 XML 里的 ph 引用可给出 idx（pandoc 对图片配文文本框会写 ph idx）
        ph_el = sh._element.find(qn("p:nvSpPr")).find(qn("p:nvPr")).find(qn("p:ph"))
        if ph_el is not None and ph_el.get("idx"):
            idx = int(ph_el.get("idx"))
        if ph_el is not None and ph_el.get("type"):
            phtype = "TITLE" if ph_el.get("type") in ("title", "ctrTitle") else phtype
        lph = layout_ph(layout, idx, phtype)
        is_title = bool(phtype == "TITLE")
        for par in sh.text_frame.paragraphs:
            lvl = par.level + 1
            runs = [r for r in par.runs if r.text.strip()]
            if not runs:
                continue
            codey = all((r.font.name or "") in MONO for r in runs)
            sizes = []
            for r in runs:
                if r.font.size is not None:
                    sizes.append(r.font.size.pt)
                else:
                    v = lvl_sz_from_lstStyle(sh.text_frame._txBody.find(qn("a:lstStyle")), lvl)
                    if v is None and lph is not None:
                        v = lvl_sz_from_lstStyle(lph._element.find(qn("p:txBody")).find(qn("a:lstStyle")), lvl)
                    if v is None:
                        v = master_sz(is_title, lvl)
                    sizes.append(v if v is not None else 18.0)
            mn = min(sizes)
            if mn < MIN and not codey:
                bad.append((si, mn, sh.name, layout.name, "".join(r.text for r in runs)[:46]))

bad.sort(key=lambda x: (x[1], x[0]))
print(f"低于 {MIN}pt 的段落数：{len(bad)}")
seen = {}
for si, sz, nm, lay, t in bad:
    seen.setdefault((lay, nm, sz), []).append(si)
for (lay, nm, sz), slides in sorted(seen.items(), key=lambda k: k[0][2]):
    print(f"  {sz:5.1f}pt  layout={lay:22s} shape={nm:22s} slides={slides[:14]}{'...' if len(slides)>14 else ''}")
