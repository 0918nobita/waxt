#!/bin/bash
set -uev

dotnet run --project test/Waxt.Location.Test
dotnet run --project test/Waxt.Lexer.Test
dotnet run --project test/Waxt.Parser.Test
dotnet run --project test
