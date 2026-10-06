param(
    [Parameter(Mandatory=$true)][string]$Pptx,
    [Parameter(Mandatory=$true)][string]$OutDir,
    [int]$Width = 1600
)
$ErrorActionPreference = "Stop"
if (-not (Test-Path $OutDir)) { New-Item -ItemType Directory -Path $OutDir | Out-Null }
$Pptx = (Resolve-Path $Pptx).Path
$OutDir = (Resolve-Path $OutDir).Path
Get-Process POWERPNT -ErrorAction SilentlyContinue | Stop-Process -Force
Start-Sleep -Milliseconds 500
$log = Join-Path $OutDir "_export.log"
$ppt = New-Object -ComObject PowerPoint.Application
$ppt.Visible = 1
$pres = $ppt.Presentations.Open($Pptx, $true, $false, $true)
$h = [int]($Width * $pres.PageSetup.SlideHeight / $pres.PageSetup.SlideWidth)
"slides=$($pres.Slides.Count) ${Width}x${h}" | Out-File $log -Encoding utf8
for ($i = 1; $i -le $pres.Slides.Count; $i++) {
    $name = Join-Path $OutDir ("slide{0:d2}.png" -f $i)
    $pres.Slides.Item($i).Export($name, "PNG", $Width, $h)
}
$pres.Close()
$ppt.Quit()
"done" | Out-File -Append $log -Encoding utf8
