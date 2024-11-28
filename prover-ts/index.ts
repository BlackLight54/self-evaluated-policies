const pl = require("tau-prolog");
import fs from "fs";


const miProgram = fs.readFileSync("../policies/prolog/meta_interpreter.pl", "utf8");
const policyProgram = fs.readFileSync("../policies/prolog/policy.pl", "utf8");
const policyMatrixProgram = fs.readFileSync("../policies/prolog/matrix.pl", "utf8");
const privateInputProgram = fs.readFileSync("../policies/prolog/input.pl", "utf8");
const session = pl.create();
const goal = "prove(inputPriceOk).";
session.consult(miProgram,
    {
        success: function () {
            console.log("Consulted MI");
        },
        error: function (err: any) {
            console.log(err);
        },
    });
session.consult(policyProgram,
    {
        success: function () {
            console.log("Consulted Policy");
        },
        error: function (err: any) {
            console.log(err);
        },
    });
session.consult(policyMatrixProgram,
    {
        success: function () {
            console.log("Consulted Matrix");
        },
        error: function (err: any) {
            console.log(err);
        },
    });
// Consult
session.consult(privateInputProgram, {
    success: function () {
        // Query
        session.query(goal, {
            success: function (goal: any ) {
                console.log("Goal added")
                // Answers
                session.answer({
                    success: function (answer: any) {
                        /* Answer */
                        console.log(answer);
                    },
                    error: function (err: any) {
                        console.log(err, JSON.stringify(err));
                    },
                    fail: function () {
                        console.log("fail");
                    },
                    limit: function () {
                        console.log("limit");
                    },
                });
            },
            error: function (err: any) {
                /* Error parsing goal */
                console.error("Error parsing goal",err);
            },
        });
    },
    error: function (err: any) {
        /* Error parsing program */
        console.log("Error consulting input",err);
    },
});