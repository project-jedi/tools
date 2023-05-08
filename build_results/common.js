const { badgen } = require("badgen");
const { XMLParser } = require("fast-xml-parser");
const mustache = require('mustache');
const fs = require('fs');

const BADGES_FOLDER = "./badges";

function saveOneBadge(filename, label, status, color)
{
    const svgContent = badgen(
        {
            label,
            status,
            color
        }
    );

    fs.writeFileSync(BADGES_FOLDER + "/" + filename + ".svg", svgContent);
}

function Process(xmlFileName, xmlRootName, badgePrefix, libraryName)
{
    const options = {
        ignoreAttributes : false,
        attributeNamePrefix : ""
    };
    const parser = new XMLParser(options);
    const data = parser.parse(fs.readFileSync("./" + xmlFileName + ".xml"), options);

    // Sort installations by target code
    const installations = data[xmlRootName].Installation;
    installations.sort((a, b) =>
        {
            const targetA = a.Target;
            const targetB = b.Target;

            const ideA = targetA[0];
            const ideB = targetB[0];
            const ideCompare = ideA.localeCompare(ideB);
            if (ideCompare != 0)
                return ideCompare;

            const versionA = Number(targetA.substr(1).split("_")[0]);
            const versionB = Number(targetB.substr(1).split("_")[0]);
            const versionCompare = versionA - versionB;

            //console.log(versionA + " - " + versionB + " = " + versionCompare);

            if (versionCompare != 0)
                return versionCompare;
            
            if (targetA < targetB)
                return -1;
            if (targetA > targetB)
                return 1;
            return 0;
        }
    )

    // Generated individual badges, storing global status along the way
    let allEnabled = true;
    let allAttempted = true; 
    let allSucceeded = true;

    let allDisabled = true;
    let allSkipped = true;
    let allFailed = true;

    if (!fs.existsSync(BADGES_FOLDER))
        fs.mkdirSync(BADGES_FOLDER);

    for (const installation of installations)
    {
        const enabled = installation.Enabled != 0;
        const attempted = installation.InstallAttempted != 0;
        const succeeded = installation.InstallSuccess != 0;

        allEnabled &= enabled;
        allAttempted &= attempted;
        allSucceeded &= succeeded;

        allDisabled &= !enabled;
        allSkipped &= !attempted;
        allFailed &= !succeeded;

        let status = "Success";
        let color = "green";
        if (!enabled)
        {
            status = "Disabled";
            color = "black";
        }
        else if (!attempted)
        {
            status = "Skipped";
            color = "grey";
        }
        else if (!succeeded)
        {
            status = "Failed";
            color = "red";
        }

        installation.LogFile = installation.LogFile?.trim();
        installation.Target = installation.Target.toLowerCase();

        saveOneBadge(badgePrefix + "_" + installation.Target.toLowerCase(), installation.TargetName, status, color);
    }

    // global status
    let status = "Success";
    let color = "green";
    if (allDisabled)
    {
        status = "All disabled";
        color = "black";
    }
    else if (allSkipped)
    {
        status = "All skipped";
        color = "grey";
    }
    else if (allFailed)
    {
        status = "All failed";
        color = "red";
    }
    else if (!allEnabled)
    {
        status = "Some were disabled";
        color = "blue";
    }
    else if (!allAttempted)
    {
        status = "Some were skipped";
        color = "cyan";
    }
    else if (!allSucceeded)
    {
        status = "Some failed";
        color = "yellow";
    }
    saveOneBadge(badgePrefix, libraryName, status, color);

    // Generated markdown page from mustache template
    const template = fs.readFileSync("./" + badgePrefix + ".mustache", "utf-8");
    const templateData = 
    {
        generated: new Date().toISOString(),
        installations
    };
    const markdown = mustache.render(template, templateData);
    fs.writeFileSync("./" + badgePrefix + ".md", markdown);

    console.log(libraryName + ": done");
}

module.exports.Process = Process;