# PPT7 课件一键构建（在 lesson02 目录下执行）
#
#   powershell -ExecutionPolicy Bypass -File ppt7_assets\build_ppt7.ps1
#
# 流程：Quarto 渲染 -> 代码/引用块后处理 -> 导出逐页预览图
# 说明：pptx 格式的 post-render 钩子不会触发，所以后处理必须显式调用；
#       直接改 PPT7.pptx 而不重新渲染时，可只运行第 2 步。

$ErrorActionPreference = "Stop"
$root = Split-Path -Parent $PSScriptRoot
Set-Location $root

$quarto   = "D:/Software/Quarto/bin/quarto.exe"
$python   = "C:/Users/Administrator/.workbuddy/binaries/python/envs/default/Scripts/python.exe"
$renv     = @{ LC_ALL = "Chinese_China.utf8" }

Write-Host "[1/3] Quarto render PPT7.qmd ..." -ForegroundColor Cyan
$env:LC_ALL = "Chinese_China.utf8"
& $quarto render PPT7.qmd

Write-Host "[2/3] 后处理：代码块 20pt / 引用块金色强调 28pt ..." -ForegroundColor Cyan
& $python "ppt7_assets/fix_pptx.py" "PPT7.pptx" 20 28

Write-Host "[2b ] 字号审计（应报告 0 处低于 28pt）..." -ForegroundColor Cyan
& $python "ppt7_assets/audit_sizes.py" "PPT7.pptx" 28

Write-Host "[3/3] 导出逐页预览图 ..." -ForegroundColor Cyan
& "$PSScriptRoot/export_pptx.ps1" -Pptx "$root/PPT7.pptx" -OutDir "$PSScriptRoot/preview_p7" -Width 1600

Write-Host "完成：PPT7.pptx / ppt7_assets/preview_p7/slide*.png" -ForegroundColor Green
