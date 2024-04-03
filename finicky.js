// https://github.com/johnste/finicky/wiki/Configuration

let { localizedName, name } = finicky.getSystemInfo();

const isWorkComputer = localizedName.startsWith('COMP_');

module.exports = {
  // All my work computers have had long generated names starting with COMP_, so
  // I use that to decide if I'm using Google Chrome or Firefox by default.
  defaultBrowser: isWorkComputer ? 'Google Chrome' : 'Firefox',
  options: {
    logRequests: true,
  },
  handlers: [
    {
      // This works in concert with the zoom rewrite section below to open Zoom
      // links in the Zoom app.
      match: /zoom\.us\/join/,
      browser: "us.zoom.xos"
    },
    {
      // Force Spotify links to actually open in the Spotify app
      match: finicky.matchDomains("open.spotify.com"),
      browser: "Spotify"
    },
    {
      // Force Discord links to open in the Discord app
      match: "https://discord.com/*",
      url: { protocol: "discord" },
      browser: "Discord",
    },
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
      browser: 'Firefox'
    },
    {
      // Open package tracking links in Firefox.
      match: finicky.matchHostnames([
        'parcel.app',
      ]),
      browser: 'Firefox'
    },
    {
      // Ensure personal projects open in Firefox.
      match: [
        'github.com/belak/*',
        'github.com/seabird-chat/*'
      ],
      browser: 'Firefox'
    },
    {
      // Open any link from Slack which doesn't match the above rules in Google Chrome. This allows us to have specific URLs opened in specific browsers and fall back to checking based on
      match: ({opener}) =>
          ["com.tinyspeck.slackmacgap"].includes(opener.bundleId),
      browser: "Google Chrome"
    },
    {
      // Open any link from Discord in Firefox
      match: ({opener}) =>
          ["com.hnc.Discord"].includes(opener.bundleId),
      browser: "Firefox"
    },
  ],

  rewrite: [
    {
      // Rewrite Zoom "join" links to be protocol links so they work properly
      // with the Zoom app.
      match: ({ url }) => url.host.includes("zoom.us") && url.pathname.includes("/j/"),
      url: ({ url }) => {
        try {
          var pass = '&pwd=' + url.search.match(/pwd=(\w*)/)[1];
        } catch {
          var pass = ""
        }
        var conf = 'confno=' + url.pathname.match(/\/j\/(\d+)/)[1];
        return {
          search: conf + pass,
          pathname: '/join',
          protocol: "zoommtg"
        }
      }
    }
  ]
}
