!function(wndw){var jadify=function(jade){return function anonymous(locals){var buf=[],locals_=locals||{},ribbon=locals_.ribbon,timeLeft=locals_.timeLeft,studentName=locals_.studentName,courseName=locals_.courseName;return buf.push('<div class="modal coursera-signature-modal-container"><div class="coursera-signature-modal-header modal-header"><button aria-hidden="true" data-modal-close="data-modal-close" class="close">×</button><img'+jade.attrs({src:ribbon,"class":["coursera-signature-modal-ribbon"]},{src:!0})+'/><p class="coursera-signature-modal-signature-track">Signature Track</p><p class="coursera-signature-modal-title">'+jade.escape(null==(jade.interp=timeLeft)?"":jade.interp)+' left to join!</p></div><div style="margin-left: 50px; margin-right: 50px;" class="coursera-signature-modal-body modal-body"><p>Hi '+jade.escape(null==(jade.interp=studentName)?"":jade.interp)+",</p><p></p><p>We are really glad to see you in "+jade.escape(null==(jade.interp=courseName)?"":jade.interp)+'! You\'ve been invited to join the Signature Track, which allows you to earn a Verified Certificate for this course.</p><p>Through this special option, you will be able to certify your success in this course by securely linking your coursework to your identity using your unique typing pattern and webcam.</p><div style="text-align: center;"><button data-modal-close="data-modal-close" class="btn coursera-signature-next-button course-signaturetrack-modal-learnmore">Learn More</button></div><div class="course-signaturetrack-modal-nothanks"><a href="javascript:void(0)" data-modal-close="data-modal-close">Not now</a></div><p style="font-size: 12px;">Note: Joining the Signature Track is optional, you can still complete the course if you decide not to join.</p></div></div>'),buf.join("")}};if("function"==typeof define&&define.amd)define(["js/lib/jade"],function(jade){return jadify(jade)});else wndw.jade.templates["spark.app.signature.js.signatureTrackLastChanceModal"]=jadify(wndw.jade.helpers)}(window);