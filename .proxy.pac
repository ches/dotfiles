function FindProxyForURL(url, host) {
    /**
     *  PERSONAL
     *  Stuff that tries to deny IPs based on region. That's lame. And
     *  Americans travel y'know.
     */
    if (dnsDomainIs(host, ".pandora.com")     ||
        dnsDomainIs(host, ".amazon.com")      ||
        dnsDomainIs(host, ".hulu.com")        ||
        dnsDomainIs(host, ".last.fm")         ||
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

