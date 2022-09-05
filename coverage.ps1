If (Test-Path test/**/TestResults) {
    Remove-Item -Recurse -Force test/**/TestResults
}

if (Test-Path coverage) {
    Remove-Item -Recurse -Force coverage
}

dotnet test --collect:"XPlat Code Coverage"

dotnet reportgenerator `
    -reports:test/**/TestResults/*/coverage.cobertura.xml `
    -targetdir:coverage `
    -reporttypes:Html `
    -assemblyfilters:-Waxt.TestUtil
