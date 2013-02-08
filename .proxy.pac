function FindProxyForURL(url, host) {
    /**
     *  RAILS MACHINE
     *  Stuff inside gateways. Use something like
     *
     *    ssh -NCfq -D 2444 you@gw01.pnap.railsmachine.net
     *
     *  to get a SOCKS proxy on localhost port 2444. Since you need
     *  a couple, using a tunnel manager app or a shell alias is helpful.
     */
    if (dnsDomainIs(host, "cacti.gnax.railsmachine.net") ||
        dnsDomainIs(host, "sflow.gnax.railsmachine.net"))
    {
        return "SOCKS localhost:3444; DIRECT";
    }
    else if (dnsDomainIs(host, "cacti.pnap.railsmachine.net")) {
        return "SOCKS localhost:2444; DIRECT";
    }

    /**
     *  PERSONAL
     *  Stuff that tries to deny IPs based on region. That's lame. And
     *  Americans travel y'know.
     */
    else if (dnsDomainIs(host, ".pandora.com")     ||
             dnsDomainIs(host, ".amazon.com")      ||
             dnsDomainIs(host, ".hulu.com")        ||
             dnsDomainIs(host, ".last.fm")         ||
             dnsDomainIs(host, ".netflix.com")     ||
             dnsDomainIs(host, ".ustream.tv")      ||
             dnsDomainIs(host, "music.google.com") ||
             dnsDomainIs(host, "*.annualcreditreport.com"))
    {
        return "SOCKS localhost:8080; DIRECT";
    }
    else {
        return "DIRECT";
    }
}

