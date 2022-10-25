// https://github.com/johnste/finicky/wiki/Configuration

let { localizedName, name } = finicky.getSystemInfo();

module.exports = {
  // All my work computers have had long generated names starting with COMP_, so
  // I use that to decide if I'm using Google Chrome or Firefox by default.
  defaultBrowser: localizedName.startsWith("COMP_") ? "Google Chrome" : "Firefox",
  handlers: [
    {
      // Open YouTube URLs in Firefox so I can have my work profile in Google,
      // and

      match: finicky.matchDomains([
        'www.youtube.com',
        'm.youtube.com',
        'youtube.com',
        'youtu.be'
      ]),
      browser: "Firefox"
    }
  ]
}