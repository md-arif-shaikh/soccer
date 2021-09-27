;;; soccer-leauges.el --- part of soccer.el, data for soccer leagues  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Md Arif Shaikh

;; Author: Md Arif Shaikh <arifshaikh.astro@gmail.com>
;; Keywords: 

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

;;; Commentary:

;; 

;;; Code:

(defvar soccer--leagues)
(setq soccer--leagues '(("England: Premier League" . (("Manchester United" . "https://www.scorespro.com/soccer/england/teams/manchester-united-MjQyNw==/")
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
						      ("Burnley" . "https://www.scorespro.com/soccer/england/teams/burnley-fc-MTA4MzQ=/")))
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
					     ("Villarreal" . "https://www.scorespro.com/soccer/spain/teams/villarreal-MTc4NDI=/")))))
(provide 'soccer-leagues)
;;; soccer-leauges.el ends here