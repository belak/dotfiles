// https://github.com/johnste/finicky/wiki/Configuration

let { localizedName, name } = finicky.getSystemInfo();

const isWorkComputer = localizedName.startsWith("COMP_");

module.exports = {
  // All my work computers have had long generated names starting with COMP_, so
  // I use that to decide if I'm using Google Chrome or Firefox by default.
  defaultBrowser: isWorkComputer ? "Google Chrome" : "Firefox",
  options: {
    logRequests: true,
  },
  handlers: [
    {
      // Open YouTube and Steam URLs in Firefox so I can have my work profile in Google,
      // and personal stuff in firefox.

      match: finicky.matchHostnames([
        'store.steampowered.com',
        'www.youtube.com',
        'm.youtube.com',
        'youtube.com',
        'youtu.be'
      ]),
      browser: "Firefox"
    },
    {
      // Open package tracking links in Firefox.
      match: finicky.matchHostnames([
        'parcel.app',
      ]),
      browser: "Firefox"
    }
  ]
}
