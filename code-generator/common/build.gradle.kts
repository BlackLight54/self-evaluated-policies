plugins {
    id("hu.bme.kotlin-application-conventions")
    antlr
}
dependencies {
    implementation("org.json:json:20230227")
    implementation("org.antlr:antlr4:4.13.1")
    antlr("org.antlr:antlr4:4.13.1")
}

tasks.named("compileKotlin") {
    dependsOn("generateGrammarSource")
}

tasks.test {
    useJUnitPlatform()
}