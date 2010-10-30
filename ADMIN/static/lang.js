function initialize() {
    for (var lang in google.language.Languages) {
        var code = google.language.Languages[lang];
        if (google.language.isTranslatable(code)) {
            var messages = new Array();
            if (!untranslated_messages[code]) {
                for (var i=0; i<all_messages.length; i++) {
                    messages.push({orig: all_messages[i]});
                }
                langs.push({lang: lang, code: code, messages: messages});
            } else if (untranslated_messages[code].length > 0) {
                for (var i=0; i<untranslated_messages[code].length; i++) {
                    messages.push({orig: untranslated_messages[code][i]});
                }
                langs.push({lang: lang, code: code, messages: messages});
            }
        }
    }
    var sortedLangs = langs.sort(function(a, b) { a.lang > b.lang ? 1 : -1 });
    var table = $("#new_language_table");
    for (var i=0; i<sortedLangs.length; i++) {
        var lang = sortedLangs[i];
        table.append('<tr><td>'+lang.lang+' ('+lang.code.toLowerCase()+')'+'</td>'+
                     '<td id="'+lang.lang.toLowerCase()+'_status">'+
                         (all_messages.length - lang.messages.length)+
                         '</td></tr>');
    }
    $("button").click(function() {
        did_alert = false;
        $(this).attr('disabled', true);
        startTrans(0, 0);
    });
}
function startTrans(lang_index, msg_index) {
    if (lang_index == langs.length) {
        return;
    }
    var lang = langs[lang_index];
    var message = lang.messages[msg_index];
    google.language.translate(message.orig, "en", lang.code, function(result) {
        if (!result.error) {
            if (result.translation) {
                message.trans = result.translation;
                var numTrans = parseInt($("#"+lang.lang.toLowerCase()+"_status").text());
                $("#"+lang.lang.toLowerCase()+"_status").text(numTrans + 1);
                if (msg_index == lang.messages.length - 1) {
                    $.ajax({
                        type: "POST",
                        url: "/admin/lang/"+lang.code+"/json",
                        data: {
                            messages: lang.messages
                        },
                        success: function(data, textStatus, xhr) {
                            $("#"+lang.lang.toLowerCase()+"_status").text("OK");
                            startTrans(lang_index+1, 0);
                        },
                        error: function(xhr, textStatus, error) {
                            alert("Error updating "+lang.lang+": "+error);
                        }
                    });
                } else {
                    setTimeout(function() {
                        startTrans(lang_index, msg_index+1);
                    }, 500);
                }
            }
        } else {
            if (!did_alert) {
                alert('Error contacting Google: '+result.error.message);
                did_alert = true;
            }
        }
    });
}

