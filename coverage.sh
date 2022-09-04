#!/bin/bash
set -uev

rm -rf test/**/TestResults coverage

dotnet test --collect:"XPlat Code Coverage"

dotnet reportgenerator \
    -reports:test/**/TestResults/*/coverage.cobertura.xml \
    -targetdir:coverage \
    -reporttypes:Html \
    -assemblyfilters:-WAXT.TestUtil
