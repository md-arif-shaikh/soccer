;;; soccer-leagues.el --- Part of soccer.el, data for soccer leagues  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Md Arif Shaikh

;; Author: Md Arif Shaikh <arifshaikh.astro@gmail.com>
;; Homepage: https://github.com/md-arif-shaikh/soccer
;; Package-Requires: ((emacs "24.1"))
;; Keywords: games
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Local variables:
;; package-lint-main-file: "soccer.el"
;; end:

;;; Commentary:
;; This contains list of urls for different clubs to get the relevant data.
;; 

;;; Code:

(defvar soccer-leagues--leagues-alist)
(setq soccer-leagues--leagues-alist '(("England: Premier League" . (("Manchester United" . "https://www.scorespro.com/soccer/england/teams/manchester-united-MjQyNw==/")
								    ("Liverpool" . "https://www.scorespro.com/soccer/england/teams/liverpool-fc-MTA5OTE=/")
								    ("Chelsea" . "https://www.scorespro.com/soccer/england/teams/chelsea-fc-MjQyMA==/")
								    ("Manchester City" . "https://www.scorespro.com/soccer/england/teams/manchester-city-MTE2NzY=/")
								    ("Everton" . "https://www.scorespro.com/soccer/england/teams/everton-fc-MTA4MTg=/")
								    ("Aston Villa" . "https://www.scorespro.com/soccer/england/teams/aston-villa-NTI1OQ==/")
								    ("Leicester" . "https://www.scorespro.com/soccer/england/teams/leicester-city-MTA4MjI=/")
								    ("Arsenal" . "https://www.scorespro.com/soccer/england/teams/arsenal-fc-MTE1NzM=/")
								    ("Tottenham" . "https://www.scorespro.com/soccer/england/teams/tottenham-MjQzMg==/")
								    ("Westham" . "https://www.scorespro.com/soccer/england/teams/west-ham-MTA4MzA=/")
								    ("Leeds" . "https://www.scorespro.com/soccer/england/teams/leeds-united-NDY4Mw==/")
								    ("Southampton" . "https://www.scorespro.com/soccer/england/teams/southampton-NTI3Mw==/")
								    ("Newcastle" . "https://www.scorespro.com/soccer/england/teams/newcastle-united-MTA4MjY=/")
								    ("Crystalpalace" . "https://www.scorespro.com/soccer/england/teams/crystal-palace-MTA4Mzc=/")
								    ("Wolves" . "https://www.scorespro.com/soccer/england/teams/wolverhampton-wanderers-fc-MjQzMw==/")
								    ("Brighton" . "https://www.scorespro.com/soccer/england/teams/brighton-MjQ2Mg==/")
								    ("Sheffield" . "https://www.scorespro.com/soccer/england/teams/sheffield-united-MTA4NDc=/")
								    ("Brentford". "https://www.scorespro.com/soccer/england/teams/brentford-fc-MTE3MTU=/")
								    ("Watford" . "https://www.scorespro.com/soccer/england/teams/watford-fc-MTA4NTE=/")
								    ("Westbrom" . "https://www.scorespro.com/soccer/england/teams/west-bromwich-MjQ1NA==/")
								    ("Fulham" . "https://www.scorespro.com/soccer/england/teams/fulham-fc-MjQyMg==/")
								    ("Burnley" . "https://www.scorespro.com/soccer/england/teams/burnley-fc-MTA4MzQ=/")
								    ("Table" . "https://www.scorespro.com/soccer/england/premier-league/standings/")))
				      ("Spain: La Liga" . (("Real Madrid" . "https://www.scorespro.com/soccer/spain/teams/real-madrid-NDE4MA==/")
							   ("Barcelona". "https://www.scorespro.com/soccer/spain/teams/fc-barcelona-MTc3NjI=/")
							   ("Atletico Madrid". "https://www.scorespro.com/soccer/spain/teams/atletico-madrid-MTgwNjM=/")
							   ("Alaves" . "https://www.scorespro.com/soccer/spain/teams/deportivo-alaves-MTc4MjQ=/")
							   ("Athletic Club" . "https://www.scorespro.com/soccer/spain/teams/athletic-bilbao-MTgwMTk=/")
							   ("Cadiz" . "https://www.scorespro.com/soccer/spain/teams/cadiz-MTc4MDM=/")
							   ("Celta Vigo" . "https://www.scorespro.com/soccer/spain/teams/celta-vigo-NDE3Mg==/")
							   ("Elche" . "https://www.scorespro.com/soccer/spain/teams/elche-MTc3ODc=/")
							   ("Espanyol" . "https://www.scorespro.com/soccer/spain/teams/real-cd-espanyol-NDE3Mw==/")
							   ("Getafe" . "https://www.scorespro.com/soccer/spain/teams/getafe-MTc5MTk=/")
							   ("Granada CF" . "https://www.scorespro.com/soccer/spain/teams/granada-cf-Mzc4MDE=/")
							   ("Levante" . "https://www.scorespro.com/soccer/spain/teams/levante-MTgwNTI=/")
							   ("Mallorca" . "https://www.scorespro.com/soccer/spain/teams/mallorca-MTgwNTQ=/")
							   ("Osasuna" . "https://www.scorespro.com/soccer/spain/teams/osasuna-NDE3OA==/")
							   ("Rayo Vallecano" . "https://www.scorespro.com/soccer/spain/teams/rayo-vallecano-MjUxOTc=/")
							   ("Real Betis" . "https://www.scorespro.com/soccer/spain/teams/real-betis-MTgwMjI=/")
							   ("Real Sociedad" . "https://www.scorespro.com/soccer/spain/teams/real-sociedad-MTgwMzI=/")
							   ("Sevilla" . "https://www.scorespro.com/soccer/spain/teams/sevilla-MjYyNTM=/")
							   ("Valencia" . "https://www.scorespro.com/soccer/spain/teams/valencia-MTgwMzc=/")
							   ("Villarreal" . "https://www.scorespro.com/soccer/spain/teams/villarreal-MTc4NDI=/")
							   ("Table" . "https://www.scorespro.com/soccer/spain/laliga/standings/")))
				      ("France: Ligue 1" . (("Paris Saint Germain" . "https://www.scorespro.com/soccer/france/teams/paris-saint-germain-MzI5NQ==/")
							    ("Lyon" . "https://www.scorespro.com/soccer/france/teams/lyon-MzI4OA==/")
							    ("Angers" . "https://www.scorespro.com/soccer/france/teams/angers-MTU0NTQ=/")
							    ("Bordeaux" . "https://www.scorespro.com/soccer/france/teams/girondins-bordeaux-MTU0Mzc=/")
							    ("Clermont Foot" . "https://www.scorespro.com/soccer/france/teams/clermont-foot-MjIxOTc=/")
							    ("Estac Troyes" . "https://www.scorespro.com/soccer/france/teams/estac-troyes-MTUxOTI=/")
							    ("Lens" . "https://www.scorespro.com/soccer/france/teams/lens-MTUxNzk=/")
							    ("Lille" . "https://www.scorespro.com/soccer/france/teams/lille-osc-MTUyNDA=/")
							    ("Lorient" . "https://www.scorespro.com/soccer/france/teams/fc-lorient-MTUyMDQ=/")
							    ("Marseille" . "https://www.scorespro.com/soccer/france/teams/olympique-marseille-MzI4OQ==/")
							    ("Metz" . "https://www.scorespro.com/soccer/france/teams/metz-MTU3MDA=/")
							    ("Monaco" . "https://www.scorespro.com/soccer/france/teams/monaco-MTU0NDQ=/")
							    ("Montpellier" . "https://www.scorespro.com/soccer/france/teams/montpellier-hsc-MTU3MDI=/")
							    ("Nantes" . "https://www.scorespro.com/soccer/france/teams/nantes-MTU0NDU=/")
							    ("Nice" . "https://www.scorespro.com/soccer/france/teams/ogc-nice-MTUxODY=/")
							    ("Reims" . "https://www.scorespro.com/soccer/france/teams/reims-MjIyMDE=/")
							    ("Rennes" . "https://www.scorespro.com/soccer/france/teams/stade-rennais-MTUxODg=/")
							    ("Saint Etienne" . "https://www.scorespro.com/soccer/france/teams/as-saint-etienne-MTU0NDk=/")
							    ("Stade Brestois 29" . "https://www.scorespro.com/soccer/france/teams/stade-brestois-29-MjM5MTI=/")
							    ("Strasbourg" . "https://www.scorespro.com/soccer/france/teams/strasbourg-MjIxOTU=/")
							    ("Table" . "https://www.scorespro.com/soccer/france/ligue-1/standings/")))))

(provide 'soccer-leagues)

;;; soccer-leagues.el ends here
