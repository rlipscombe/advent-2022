$ErrorActionPreference = 'Stop'

$path = [System.IO.Path]::Combine($PWD, "..\input11.txt")
$lines = [System.IO.File]::ReadAllLines($path)

# Parser
# Monkey 2:
#   Starting items: 79, 60, 97
#   Operation: new = old * old
#   Test: divisible by 13
#     If true: throw to monkey 1
#     If false: throw to monkey 3

$monkeys = @()

$monkey = @{}
$monkey.Inspections = 0
$monkey.Items = [System.Collections.ArrayList](@())

foreach ($line in $lines) {
    if ($line -match 'Monkey (\d+):') {
        $monkey.Id = $Matches.1
    }
    elseif ($line -match 'Starting items: (.*)') {
        $items = $Matches.1
        $items = @($items.Split(", ") | ForEach-Object { [int]$_ })
        $monkey.Items.AddRange($items)
    }
    elseif ($line -match 'Operation: new = old \* old') {
        $monkey.Operation = {
            param($worry)
            ($worry * $worry)
        }
    }
    elseif ($line -match 'Operation: new = old \+ (\d+)') {
        $value = $Matches.1
        $monkey.Operation = {
            param($worry)
            ($worry + $value)
        }.GetNewClosure()
    }
    elseif ($line -match 'Operation: new = old \* (\d+)') {
        $value = $Matches.1
        $monkey.Operation = {
            param($worry)
            ($worry * $value)
        }.GetNewClosure()
    }
    elseif ($line -match 'Test: divisible by (\d+)') {
        $monkey.Divisor = $Matches.1
    }
    elseif ($line -match 'If true: throw to monkey (\d+)') {
        $monkey.IfTrue = $Matches.1
    }
    elseif ($line -match 'If false: throw to monkey (\d+)') {
        $monkey.IfFalse = $Matches.1
    }
    elseif ($line.Length -eq 0) {
        $monkeys += , $monkey
        $monkey = @{}
        $monkey.Inspections = 0
        $monkey.Items = [System.Collections.ArrayList](@())
    }
    else {
        throw $line
    }
}

$monkeys += , $monkey
# Write-Output $monkeys

$rounds = 10000

$common = 1
foreach ($monkey in $monkeys) {
    $common *= $monkey.Divisor
}
Write-Output $common
function Update-Worry {
    param($worry)
    $worry % $common
    # [int][Math]::Floor($worry / 3)
}

for ($round = 1; $round -le $rounds; $round++) {
    if (($round % 100) -eq 0) { Write-Output ("{0} / {1}" -f $round, $rounds) }
    foreach ($monkey in $monkeys) {
        while ($monkey.Items.Length) {
            $item = $monkey.Items[0]
            $monkey.Items.RemoveAt(0)

            # Write-Output ("Monkey inspects an item with a worry level of {0}." -f $item)
            $monkey.Inspections++
            $item = &$monkey.Operation $item
            # Write-Output ("...to {0}" -f $item)

            $item = (Update-Worry $item)
            # Write-Output ("...to {0}" -f $item)

            $remainder = ($item % $monkey.Divisor)

            if ($remainder -eq 0) {
                # Write-Output ("true -> {0}" -f $monkey.IfTrue)
                $monkeys[$monkey.IfTrue].Items.Add($item) | Out-Null
            }
            else {
                # Write-Output ("false -> {0}" -f $monkey.IfFalse)
                $monkeys[$monkey.IfFalse].Items.Add($item) | Out-Null
            }
        }
    }

    # foreach ($monkey in $monkeys) {
    #     Write-Output ("{0}: {1}" -f $monkey.Id, ($monkey.Items -join ", "))
    # }
}

# foreach ($monkey in $sorted) {
#     Write-Output ("{0}: {1}" -f $monkey.Id, $monkey.Inspections)
# }

$sorted = $monkeys | Sort-Object Inspections -Descending
Write-Output ($sorted[0].Inspections * $sorted[1].Inspections)
