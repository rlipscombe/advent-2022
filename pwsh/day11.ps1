$path = [System.IO.Path]::Combine($PWD, "..\example11.txt")
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
foreach ($line in $lines) {
    if ($line -match 'Monkey (\d+):') {
        $monkey.Id = $Matches.1
    }
    elseif ($line -match 'Starting items: (.*)') {
        $items = $Matches.1
        $monkey.Items = $items.Split(", ")
    }
    elseif ($line -match 'Operation: new = old \* old') {
        $monkey.Operation = {
            param { $worry }
            Write-Output $worry * $worry
        }
    }
    elseif ($line -match 'Operation: new = old \+ (\d+)') {
        $value = $Matches.1
        $monkey.Operation = {
            param { $worry }
            Write-Output $worry + $value
         }.GetNewClosure()
    }
    elseif ($line -match 'Operation: new = old \* (\d+)') {
        $value = $Matches.1
        $monkey.Operation = {
            param { $worry }
            Write-Output $worry * $value
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
    }
    else {
        throw $line
    }
}

$monkeys += , $monkey
Write-Output $monkeys
