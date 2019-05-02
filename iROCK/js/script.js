$(function () {
    function base64Encode( stringInput ) {
 
        // NOTE: This normalization technique for handling characters that require
        // more than an 8-bit representation was provided on the Mozilla Developer
        // Network website for Base64 encoding. They also provided a companion DECODE
        // method. But, we don't need to decode in this demo.
        // --
        // READ MORE: https://developer.mozilla.org/en-US/docs/Web/API/WindowBase64/Base64_encoding_and_decoding#The_Unicode_Problem
        var normalizedInput = encodeURIComponent( stringInput ).replace(
            /%([0-9A-F]{2})/g,
            function toSolidBytes( $0, hex ) {

                return( String.fromCharCode( "0x" + hex ) );

            }
        );

        return( btoa( normalizedInput ) );

    }

    function b64DecodeUnicode(str) {
        // Going backwards: from bytestream, to percent-encoding, to original string.
        return decodeURIComponent(atob(str).split('').map(function (c) {
            return '%' + ('00' + c.charCodeAt(0).toString(16)).slice(-2);
        }).join(''));
    }


    $('.files').mouseleave(function () {
        $(this).animate({ top: -$(this).height() + 15 }, null, 'easeOutBounce');
    }).mouseenter(function () {
        $(this).animate({ top: 0 }, null, 'easeOutBounce');
    });

    /*
    var resize;
    function headerSize(event) {
        let $this = $(this);
        resize && clearInterval(resize);
        resize = setTimeout(function () {
            $('content').animate({ marginTop: $this.closest('header').height() + 25 });
            console.log($this.closest('header').height());
        }, 50);
    }

    $(window).on('resize', headerSize.bind($('header'))).trigger('resize');
    */

    $('header').on('DOMNodeInserted', '[data-target=".tags"],[data-target=".breaks"]', function (event) {
        let $this = $(this);
        //headerSize.apply(this, event);
        $this.draggable({
            appendTo: 'body',
            cursor: 'move',
            helper: "clone",
            revert: 'invalid'
        })
    });

    $("content").on('DOMNodeInserted', '.line', function (event) {
        $(this).droppable({
            accept: function ($this) {
                return !$(this).find('.tags').has('[data-id="' + String($this.data('id') || "").replace(/"/,'\\\\"') + '"]').length;
            },
            drop: function (event, ui) {
                var $this = ui.draggable;
                var $thesetags = $(this).find($this.data('target'));
                $this.clone().appendTo($thesetags).one('click', function () {
                    $(this).remove();
                });
            }
        });

        
    });

    $('.download').on('click', function () {
        blob = new Blob(
            [ $('content').text().replace(/\s*$/gm, "") ],
            {
                type : "text/plain;charset=utf-8"
            }
        );
        $(this).attr('href', URL.createObjectURL(blob));
    });

    $('input').on('change', function () {
        if (!window.File || !window.FileReader || !window.FileList || !window.Blob) {
            alert('The File APIs are not fully supported in this browser.');
            return;
        }

        input = $(this).get(0);
        if (!input) {
            alert("Um, couldn't find the fileinput element.");
        }
        else if (!input.files) {
            alert("This browser doesn't seem to support the `files` property of file inputs.");
        }
        else if (!input.files[0]) {
            alert("Please select a file before clicking 'Load'");
        }
        else {
            
            file = input.files[0];
            fr = new FileReader();
            
            $this = $(this);
            $target = $($this.attr('name'));

            fr.onload = function () {
                var data = b64DecodeUnicode(this.result.split(',')[1]);
                // console.log($my, data);
                data = data.replace(/\s*$/gm, "").split(/\r?\n/g).map(function (i) {
                    var $line = $($this.data('template'));

                    if ($this.hasClass('file')) {
                        $('.download').attr('download', $this.val().replace(/^.*[\/\\](.*?)$/g, '$1')).removeClass('disabled');
                    }
                    
                    if ($this.data('target')) {
                      if (i.length == 0) {
                            $line.find($this.data('target')).html("&nbsp;");
                      } else {
                            $line.find($this.data('target')).text(i);
                      }
                    }

                    if ($this.data('id')) {
                        $line.attr($this.data('id'), i);
                        if ($this.hasClass('tags'))
                            $line.text(' ' + i);
                        else 
                            $line.text("\n" + i);
                    }
                    
                    var html = $('<div/>').append($line).html() + "\n";
                    //console.log(html);  
                    return html;
                }).join('');
                //console.log(data);
                $target.html(data);
            };
            //fr.readAsText(file);
            fr.readAsDataURL(file);

            //for (i=0;i<100;i++) $('header').append('<span data-id="'+i+'" data-target=".tags"></span>')
        }
    });
});