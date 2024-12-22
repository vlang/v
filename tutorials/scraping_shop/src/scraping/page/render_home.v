module page

import veb
import shareds.wcontext
import shareds.components.badge

const badges = [
	{
		'status': 'success'
		'icon':   '/assets/icons/validate-svgrepo-com.svg'
		'label':  'Amazon Livros'
	},
	{
		'status': 'success'
		'icon':   '/assets/icons/validate-svgrepo-com.svg'
		'label':  'InstantGames'
	},
	{
		'status': 'failure'
		'icon':   '/assets/icons/invalide-svgrepo-com.svg'
		'label':  'EpicGames'
	},
	{
		'status': 'failure'
		'icon':   '/assets/icons/invalide-svgrepo-com.svg'
		'label':  'Steam'
	},
]

fn render_home(mut ctx wcontext.WsCtx) veb.RawHtml {
	badges_plataforms := badge.list_badge_to_html(badges)

	return $tmpl('\\view\\home.html')
}
