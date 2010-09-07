% ==========================================================================================================
% MISULTIN - Main
%
% >-|-|-(°>
% 
% Copyright (C) 2010, Roberto Ostinelli <roberto@ostinelli.net>.
% All rights reserved.
%
% BSD License
% 
% Redistribution and use in source and binary forms, with or without modification, are permitted provided
% that the following conditions are met:
%
%  * Redistributions of source code must retain the above copyright notice, this list of conditions and the
%	 following disclaimer.
%  * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and
%	 the following disclaimer in the documentation and/or other materials provided with the distribution.
%  * Neither the name of the authors nor the names of its contributors may be used to endorse or promote
%	 products derived from this software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
% WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
% PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
% ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
% POSSIBILITY OF SUCH DAMAGE.
% ==========================================================================================================
-module(misultin_utility).
-vsn("0.6.0").

% API
-export([get_http_status_code/1, get_content_type/1, get_key_value/2]).


% ============================ \/ API ======================================================================

% Function: HttpStatus
% Description: Returns a complete HTTP header
% most common first
get_http_status_code(200) ->
	"HTTP/1.1 200 OK\r\n";
get_http_status_code(100) ->
	"HTTP/1.1 100 Continue\r\n";
get_http_status_code(101) ->
	"HTTP/1.1 101 Switching Protocols\r\n";
get_http_status_code(301) ->
	"HTTP/1.1 301 Moved Permanently\r\n";
get_http_status_code(400) ->
	"HTTP/1.1 400 Bad Request\r\n";
get_http_status_code(401) ->
	"HTTP/1.1 401 Unauthorized\r\n";	
get_http_status_code(403) ->
	"HTTP/1.1 403 Forbidden\r\n";
get_http_status_code(404) ->
	"HTTP/1.1 404 Not Found\r\n";				
get_http_status_code(408) ->
	"HTTP/1.1 408 Request Timeout\r\n";			
get_http_status_code(500) ->
	"HTTP/1.1 500 Internal Server Error\r\n";
get_http_status_code(501) ->
	"HTTP/1.1 501 Not Implemented\r\n";				
% less common last
get_http_status_code(201) ->
	"HTTP/1.1 201 Created\r\n";
get_http_status_code(202) ->
	"HTTP/1.1 202 Accepted\r\n";
get_http_status_code(203) ->
	"HTTP/1.1 203 Non-Authoritative Information\r\n";
get_http_status_code(204) ->
	"HTTP/1.1 204 No Content\r\n";
get_http_status_code(205) ->
	"HTTP/1.1 205 Reset Content\r\n";
get_http_status_code(206) ->
	"HTTP/1.1 206 Partial Content\r\n";
get_http_status_code(300) ->
	"HTTP/1.1 300 Multiple Choices\r\n";
get_http_status_code(302) ->
	"HTTP/1.1 302 Found\r\n";
get_http_status_code(303) ->
	"HTTP/1.1 303 See Other\r\n";
get_http_status_code(304) ->
	"HTTP/1.1 304 Not Modified\r\n";
get_http_status_code(305) ->
	"HTTP/1.1 305 Use Proxy\r\n";
get_http_status_code(307) ->
	"HTTP/1.1 307 Temporary Redirect\r\n";
get_http_status_code(402) ->
	"HTTP/1.1 402 Payment Required\r\n";
get_http_status_code(405) ->
	"HTTP/1.1 405 Method Not Allowed\r\n";
get_http_status_code(406) ->
	"HTTP/1.1 406 Not Acceptable\r\n";
get_http_status_code(407) ->
	"HTTP/1.1 407 Proxy Authentication Required\r\n";
get_http_status_code(409) ->
	"HTTP/1.1 409 Conflict\r\n";
get_http_status_code(410) ->
	"HTTP/1.1 410 Gone\r\n";
get_http_status_code(411) ->
	"HTTP/1.1 411 Length Required\r\n";
get_http_status_code(412) ->
	"HTTP/1.1 412 Precondition Failed\r\n";
get_http_status_code(413) ->
	"HTTP/1.1 413 Request Entity Too Large\r\n";
get_http_status_code(414) ->
	"HTTP/1.1 414 Request-URI Too Long\r\n";
get_http_status_code(415) ->
	"HTTP/1.1 415 Unsupported Media Type\r\n";
get_http_status_code(416) ->
	"HTTP/1.1 416 Requested Range Not Satisfiable\r\n";
get_http_status_code(417) ->
	"HTTP/1.1 417 Expectation Failed\r\n";
get_http_status_code(502) ->
	"HTTP/1.1 502 Bad Gateway\r\n";
get_http_status_code(503) ->
	"HTTP/1.1 503 Service Unavailable\r\n";
get_http_status_code(504) ->
	"HTTP/1.1 504 Gateway Timeout\r\n";
get_http_status_code(505) ->
	"HTTP/1.1 505 HTTP Version Not Supported\r\n";
get_http_status_code(Other) ->
	lists:flatten(io_lib:format("HTTP/1.1 ~p \r\n", [Other])).

% get content type
get_content_type(FileName) ->
	case filename:extension(FileName) of
		% most common first
		".doc" -> "application/msword";
		".exe" -> "application/octet-stream";
		".pdf" -> "application/pdf";
		".rtf" -> "application/rtf";
		".ppt" -> "application/vnd.ms-powerpoint";
		".tgz" -> "application/x-compressed";
		".tar" -> "application/x-tar";
		".zip" -> "application/zip";
		".mp3" -> "audio/mpeg";
		".wav" -> "audio/x-wav";
		".bmp" -> "image/bmp";
		".ram" -> "audio/x-pn-realaudio";
		".gif" -> "image/gif";
		".jpe" -> "image/jpeg";
		".jpeg" -> "image/jpeg";
		".jpg" -> "image/jpeg";
		".tif" -> "image/tiff";
		".tiff" -> "image/tiff";
		".htm" -> "text/html";
		".html" -> "text/html";
		".txt" -> "text/plain";
		".mp2" -> "video/mpeg";
		".mpa" -> "video/mpeg";
		".mpe" -> "video/mpeg";
		".mpeg" -> "video/mpeg";
		".mpg" -> "video/mpeg";
		".mov" -> "video/quicktime";
		".avi" -> "video/x-msvideo";	
		% less common last
		".evy" -> "application/envoy";
		".fif" -> "application/fractals";
		".spl" -> "application/futuresplash";
		".hta" -> "application/hta";
		".acx" -> "application/internet-property-stream";
		".hqx" -> "application/mac-binhex40";
		".dot" -> "application/msword";
		".bin" -> "application/octet-stream";
		".class" -> "application/octet-stream";
		".dms" -> "application/octet-stream";
		".lha" -> "application/octet-stream";
		".lzh" -> "application/octet-stream";
		".oda" -> "application/oda";
		".axs" -> "application/olescript";
		".prf" -> "application/pics-rules";
		".p10" -> "application/pkcs10";
		".crl" -> "application/pkix-crl";
		".ai" -> "application/postscript";
		".eps" -> "application/postscript";
		".ps" -> "application/postscript";
		".setpay" -> "application/set-payment-initiation";
		".setreg" -> "application/set-registration-initiation";
		".xla" -> "application/vnd.ms-excel";
		".xlc" -> "application/vnd.ms-excel";
		".xlm" -> "application/vnd.ms-excel";
		".xls" -> "application/vnd.ms-excel";
		".xlt" -> "application/vnd.ms-excel";
		".xlw" -> "application/vnd.ms-excel";
		".msg" -> "application/vnd.ms-outlook";
		".sst" -> "application/vnd.ms-pkicertstore";
		".cat" -> "application/vnd.ms-pkiseccat";
		".stl" -> "application/vnd.ms-pkistl";
		".pot" -> "application/vnd.ms-powerpoint";
		".pps" -> "application/vnd.ms-powerpoint";
		".mpp" -> "application/vnd.ms-project";
		".wcm" -> "application/vnd.ms-works";
		".wdb" -> "application/vnd.ms-works";
		".wks" -> "application/vnd.ms-works";
		".wps" -> "application/vnd.ms-works";
		".hlp" -> "application/winhlp";
		".bcpio" -> "application/x-bcpio";
		".cdf" -> "application/x-cdf";
		".z" -> "application/x-compress";
		".cpio" -> "application/x-cpio";
		".csh" -> "application/x-csh";
		".dcr" -> "application/x-director";
		".dir" -> "application/x-director";
		".dxr" -> "application/x-director";
		".dvi" -> "application/x-dvi";
		".gtar" -> "application/x-gtar";
		".gz" -> "application/x-gzip";
		".hdf" -> "application/x-hdf";
		".ins" -> "application/x-internet-signup";
		".isp" -> "application/x-internet-signup";
		".iii" -> "application/x-iphone";
		".js" -> "application/x-javascript";
		".latex" -> "application/x-latex";
		".mdb" -> "application/x-msaccess";
		".crd" -> "application/x-mscardfile";
		".clp" -> "application/x-msclip";
		".dll" -> "application/x-msdownload";
		".m13" -> "application/x-msmediaview";
		".m14" -> "application/x-msmediaview";
		".mvb" -> "application/x-msmediaview";
		".wmf" -> "application/x-msmetafile";
		".mny" -> "application/x-msmoney";
		".pub" -> "application/x-mspublisher";
		".scd" -> "application/x-msschedule";
		".trm" -> "application/x-msterminal";
		".wri" -> "application/x-mswrite";
		".nc" -> "application/x-netcdf";
		".pma" -> "application/x-perfmon";
		".pmc" -> "application/x-perfmon";
		".pml" -> "application/x-perfmon";
		".pmr" -> "application/x-perfmon";
		".pmw" -> "application/x-perfmon";
		".p12" -> "application/x-pkcs12";
		".pfx" -> "application/x-pkcs12";
		".p7b" -> "application/x-pkcs7-certificates";
		".spc" -> "application/x-pkcs7-certificates";
		".p7r" -> "application/x-pkcs7-certreqresp";
		".p7c" -> "application/x-pkcs7-mime";
		".p7m" -> "application/x-pkcs7-mime";
		".p7s" -> "application/x-pkcs7-signature";
		".sh" -> "application/x-sh";
		".shar" -> "application/x-shar";
		".swf" -> "application/x-shockwave-flash";
		".sit" -> "application/x-stuffit";
		".sv4cpio" -> "application/x-sv4cpio";
		".sv4crc" -> "application/x-sv4crc";
		".tcl" -> "application/x-tcl";
		".tex" -> "application/x-tex";
		".texi" -> "application/x-texinfo";
		".texinfo" -> "application/x-texinfo";
		".roff" -> "application/x-troff";
		".t" -> "application/x-troff";
		".tr" -> "application/x-troff";
		".man" -> "application/x-troff-man";
		".me" -> "application/x-troff-me";
		".ms" -> "application/x-troff-ms";
		".ustar" -> "application/x-ustar";
		".src" -> "application/x-wais-source";
		".cer" -> "application/x-x509-ca-cert";
		".crt" -> "application/x-x509-ca-cert";
		".der" -> "application/x-x509-ca-cert";
		".pko" -> "application/ynd.ms-pkipko";
		".au" -> "audio/basic";
		".snd" -> "audio/basic";
		".mid" -> "audio/mid";
		".rmi" -> "audio/mid";
		".aif" -> "audio/x-aiff";
		".aifc" -> "audio/x-aiff";
		".aiff" -> "audio/x-aiff";
		".m3u" -> "audio/x-mpegurl";
		".ra" -> "audio/x-pn-realaudio";
		".cod" -> "image/cis-cod";
		".ief" -> "image/ief";
		".jfif" -> "image/pipeg";
		".svg" -> "image/svg+xml";
		".ras" -> "image/x-cmu-raster";
		".cmx" -> "image/x-cmx";
		".ico" -> "image/x-icon";
		".pnm" -> "image/x-portable-anymap";
		".pbm" -> "image/x-portable-bitmap";
		".pgm" -> "image/x-portable-graymap";
		".ppm" -> "image/x-portable-pixmap";
		".rgb" -> "image/x-rgb";
		".xbm" -> "image/x-xbitmap";
		".xpm" -> "image/x-xpixmap";
		".xwd" -> "image/x-xwindowdump";
		".mht" -> "message/rfc822";
		".mhtml" -> "message/rfc822";
		".nws" -> "message/rfc822";
		".css" -> "text/css";
		".323" -> "text/h323";
		".stm" -> "text/html";
		".uls" -> "text/iuls";
		".bas" -> "text/plain";
		".c" -> "text/plain";
		".h" -> "text/plain";
		".rtx" -> "text/richtext";
		".sct" -> "text/scriptlet";
		".tsv" -> "text/tab-separated-values";
		".htt" -> "text/webviewhtml";
		".htc" -> "text/x-component";
		".etx" -> "text/x-setext";
		".vcf" -> "text/x-vcard";
		".mpv2" -> "video/mpeg";
		".qt" -> "video/quicktime";
		".lsf" -> "video/x-la-asf";
		".lsx" -> "video/x-la-asf";
		".asf" -> "video/x-ms-asf";
		".asr" -> "video/x-ms-asf";
		".asx" -> "video/x-ms-asf";
		".movie" -> "video/x-sgi-movie";
		".flr" -> "x-world/x-vrml";
		".vrml" -> "x-world/x-vrml";
		".wrl" -> "x-world/x-vrml";
		".wrz" -> "x-world/x-vrml";
		".xaf" -> "x-world/x-vrml";
		".xof" -> "x-world/x-vrml";
		_ -> "application/octet-stream"
	end.

% faster than proplists:get_value
get_key_value(Key, List)->
	case lists:keyfind(Key, 1, List) of
		false-> undefined;
		{_K, Value}-> Value
	end.

% ============================ /\ API ======================================================================



% ============================ \/ INTERNAL FUNCTIONS =======================================================

% ============================ /\ INTERNAL FUNCTIONS =======================================================
