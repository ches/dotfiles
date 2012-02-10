// .mongorc.js is executed on mongo shell initialization in MongoDB 1.9.1+

// Set prompt to database@hostname>
host = db.serverStatus().host;
prompt = function() {
    return db + "@" + host + "> ";
};

