use strict;
use 5.6.1;
use Irssi;
use Irssi::TextUI;
use POSIX qw(ceil floor);
use vars qw($VERSION %IRSSI);
 
my $nicklist_visible = 0;
my $nicklist_nick_width=8;
my $nicklist_window=undef;
$VERSION=0.13;

%IRSSI = (
    authors     => 'heeen',
    contact     => 'heeen@gmx.de',
    name        => 'nickbar',
    description => 'Adds a nicklist to irssi',
    license     => 'GNU GPLv2 or later',
    url         => 'none yet'
);

Irssi::command_bind( 'nicklist','start');
Irssi::settings_add_int('misc', 'nicklist_nick_width', 9);
Irssi::settings_add_int('misc', 'nicklist_max_lines', 10);
Irssi::settings_add_str('misc', 'nicklist_nick_hilights', '');
Irssi::settings_add_int('misc', 'nicklist_window_num', 99);

Irssi::signal_add_last('window changed', 'event_window_changed');
Irssi::signal_add_last("nick mode changed", "event_modechange"); 
Irssi::signal_add_last("message join", "event_joinpartkick"); 
Irssi::signal_add_last("message part", "event_joinpartkick"); 
Irssi::signal_add_last("message kick", "event_joinpartkick"); 
Irssi::signal_add_last("message quit", "event_quit"); 
Irssi::signal_add_last("setup changed", "update_nicklist"); 


start();

sub start()
{
	$nicklist_window=Irssi::window_find_name("nicklist");
	my $window_num = Irssi::settings_get_int('nicklist_window_num');
	
	if(!$nicklist_window)
	{
		my $lastwin=Irssi::active_win();
		$nicklist_window=Irssi::Windowitem::window_create(0,0);
		$nicklist_window->set_name("nicklist");
		$nicklist_window->set_history ("nicklist");
		$nicklist_window->set_refnum($window_num);
		$lastwin->set_active();
		Irssi::print("New Window");
		$lastwin->command("window show nicklist");
	}
	update_nicklist();
}

sub event_window_changed()
{
	update_nicklist();
}
sub event_modechange()
{
	my ($channel)=@_;
	return unless is_channel(Irssi::active_win()->{active});
	if(Irssi::active_win()->{active}->{name} eq $channel->{name})
	{	
	update_nicklist();
	}
}

sub event_joinpartkick()
{
	my ($server, $channel)=@_;
	return unless is_channel(Irssi::active_win()->{active});
	if(Irssi::active_win()->{active}->{name} eq $channel)
	{	
	update_nicklist();
	}
}

sub event_quit()
{
	my ($server, $nick)=@_;
	return unless is_channel(Irssi::active_win()->{active});
	if(Irssi::active_win()->{active}->nick_find($nick))
	{	
	update_nicklist();

	}
}



sub update_nicklist()
{
	$nicklist_window=Irssi::window_find_name("nicklist");
	$nicklist_nick_width=Irssi::settings_get_int('nicklist_nick_width');
	if(!$nicklist_window)
	{
		Irssi::print("nicklist window closed, /nicklist to re-create");
		return;
	}
	$nicklist_window->command("scrollback clear");
	my $channel = Irssi::active_win->{active};
	if (is_channel($channel)) 
	{
		my $nicks;
		my $hilightnicks=Irssi::settings_get_str('nicklist_nick_hilights');
		my $colums=floor($nicklist_window->{width}/($nicklist_nick_width+3));
		my $col=0;
		my $rows=0;
		my $max_rows=Irssi::settings_get_int('nicklist_max_lines');
		my @modes=('%r@','%%','%g+',' ');
		my @nicks = 
		map {my ($mode,$nick)=$_=~/(\d)(.+)/;
			my $shortnick=substr($nick.(' 'x$nicklist_nick_width),0,$nicklist_nick_width-1);
			if($hilightnicks=~/\Q$nick\E/)
				{$shortnick='%W'.$shortnick;}
			else
				{$shortnick='%N'.$shortnick;}
			$shortnick=$modes[$mode].$shortnick;
			$shortnick;
			} 
		sort map {
			if		($_->{op})		{'0'.$_->{nick};}
			elsif	($_->{halfop})	{'1'.$_->{nick};}
			elsif	($_->{voice})	{'2'.$_->{nick};}
			else					{'3'.$_->{nick};}
			} $channel->nicks();
		
		foreach (@nicks) 
		{
			$nicks.="%K[".$_."%K] ";$col++;
			if($col==$colums)
			{
				$nicklist_window->print($nicks,MSGLEVEL_NEVER);
				$nicks="";
				$col=0;
				$rows++;
			}
		}
		if($nicks){$nicklist_window->print($nicks,MSGLEVEL_NEVER);$rows++;}
		$rows=2 unless $rows>2;
		$rows=$max_rows if $rows>$max_rows;
		$nicklist_window->command("window size $rows");
		$nicklist_window->command("sb home");
	}
	else
	{
		$nicklist_window->command("window size 2");
		$nicklist_window->print("This is the nicklist window, type /wc ".$nicklist_window->{refnum}." to close.");
		$nicklist_window->print("Active window is not a channel.");
	}
}

sub is_channel()
{
	my ($channel) = @_;
	if ($channel 
			&& (ref($channel) eq 'Irssi::Irc::Channel' || ref($channel) eq 'Irssi::Silc::Channel')
			&& $channel->{'type'} eq 'CHANNEL' 
			&& ($channel->{chat_type} eq 'SILC' || $channel->{'names_got'}) )
	{return 1;}
	
	return 0;
}
