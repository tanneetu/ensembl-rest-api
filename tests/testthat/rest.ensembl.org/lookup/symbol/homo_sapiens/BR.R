structure(list(method = "GET", url = "https://rest.ensembl.org/lookup/symbol/homo_sapiens/BR", 
    status_code = 400L, headers = structure(list(Vary = "Content-Type", 
        Vary = "Origin", `Content-Type` = "application/json", 
        Date = "Thu, 06 Mar 2025 14:46:40 GMT", `X-RateLimit-Limit` = "55000", 
        `X-RateLimit-Reset` = "800", `X-Runtime` = "2.836247", 
        Connection = "close", `X-RateLimit-Period` = "3600", 
        `X-RateLimit-Remaining` = "54989", `Content-Length` = "47"), class = "httr2_headers"), 
    body = charToRaw("{\"error\":\"No valid lookup found for symbol BR\"}"), 
    cache = new.env(parent = emptyenv())), class = "httr2_response")
