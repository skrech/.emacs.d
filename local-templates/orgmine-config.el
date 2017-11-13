;;; orgmine-config --- Local config of orgmine

;;; Commentary:
;; Make a local configuration to orgmine package.

;;; Code:
(setq orgmine-servers
	'(("redmine"                      ; server name for this entry
	   (host . "http://redmine.example.com")
	   (api-key . "blabblabblab")	; your Redmine REST API key
	   (issue-title-format . "[[redmine:issues/%{id}][#%{id}]] %{subject}")
	   (journal-title-format . "[[redmine:issues/%{id}#note-%{count}][V#%{id}-%{count}]] %{created_on} %{author}")
	   (version-title-format . "[[redmine:versions/%{id}][V#%{id}]] %{name}")
	   (tracker-title-format . "%{name}")
	   (project-title-format . "[[redmine:projects/%{identifier}][%{identifier}]] %{name}")
	   (user-name-format . "%{firstname} %{lastname}"))))

(provide 'orgmine-config)

;;; orgmine-config.el ends here
