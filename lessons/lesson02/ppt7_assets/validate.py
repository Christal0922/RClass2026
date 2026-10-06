# -*- coding: utf-8 -*-
"""交付前校验：底部越界检测 + 生成 6 张预览联页图"""
from PIL import Image, ImageDraw
import glob, os, math

files = sorted(glob.glob("preview_p7/slide*.png"))
bad = []
for f in files:
    im = Image.open(f).convert("RGB"); W, H = im.size; px = im.load()
    c = 0
    for y in range(H-30, H):
        for x in range(0, W, 3):
            r, g, b = px[x, y]
            if abs(r-16)+abs(g-24)+abs(b-32) > 60:
                c += 1
    if c > 3 and os.path.basename(f) != "slide01.png":
        bad.append((os.path.basename(f), c))
print("底部越界页：", bad if bad else "无")
print("总页数：", len(files))

cols, per = 3, 15
tw = 500
for pg in range(math.ceil(len(files)/per)):
    chunk = files[pg*per:(pg+1)*per]
    ims = [Image.open(f) for f in chunk]
    th = int(tw * ims[0].height / ims[0].width)
    R = math.ceil(len(chunk)/cols)
    cv = Image.new("RGB", (cols*(tw+8)+8, R*(th+22)+8), (255, 255, 255))
    d = ImageDraw.Draw(cv)
    for i, (f, im) in enumerate(zip(chunk, ims)):
        r, c = divmod(i, cols)
        x, y = 8+c*(tw+8), 8+r*(th+22)
        cv.paste(im.resize((tw, th)), (x, y))
        d.text((x+4, y+th+4), os.path.basename(f).replace("slide", "第").replace(".png", "页"), fill=(0, 0, 0))
    cv.save(f"preview/ppt7_p{pg+1}.png")
print("预览联页：preview/ppt7_p1..p6.png")
