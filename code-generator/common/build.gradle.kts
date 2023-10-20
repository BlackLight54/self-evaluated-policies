plugins {
    id("hu.bme.kotlin-application-conventions")
}
dependencies {
    implementation("org.json:json:20230227")
}

tasks.test {
    useJUnitPlatform()
}