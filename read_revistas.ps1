$ErrorActionPreference = "Stop"
$excel = New-Object -ComObject Excel.Application
$excel.Visible = $false
$excel.DisplayAlerts = $false
try {
    $wb = $excel.Workbooks.Open("C:\Users\danie\OneDrive\Escritorio\Natura\FPE\paper\revistas.xlsx", 0, $true)
    $ws = $wb.Worksheets.Item(1)
    $used = $ws.UsedRange
    Write-Host ("Rows: " + $used.Rows.Count + " Cols: " + $used.Columns.Count)
    $arr = $used.Value2
    $outPath = "C:\Users\danie\AppData\Local\Temp\claude\C--Users-danie-OneDrive-Escritorio-Natura\de999c94-cf3d-4efa-ae6d-7617cfeedc74\scratchpad\xlsx_build\revistas_current.csv"
    $sb = New-Object System.Text.StringBuilder
    for ($r=1; $r -le $used.Rows.Count; $r++) {
        $line = @()
        for ($c=1; $c -le $used.Columns.Count; $c++) {
            $v = $arr[$r,$c]
            if ($null -eq $v) { $v = "" }
            $v = $v -replace '"','""'
            $line += ('"' + $v + '"')
        }
        [void]$sb.AppendLine(($line -join ","))
    }
    [System.IO.File]::WriteAllText($outPath, $sb.ToString(), (New-Object System.Text.UTF8Encoding($true)))
    Write-Host ("Saved: " + $outPath)
    $wb.Close($false)
} finally {
    $excel.Quit()
    [System.Runtime.Interopservices.Marshal]::ReleaseComObject($excel) | Out-Null
}
