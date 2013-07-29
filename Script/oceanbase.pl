#!/usr/bin/perl -w

# Author: 符风 <fufeng.syd@alipay.com>
#
# Todo:
# 1. remove dependence of oceanbase root dir. [DONE]
# 2. promote precise of envrionment checking and command running.
# 3. make indenpendence of linux user
# 4. ...

use Data::Dumper;
use Getopt::Long;
use Pod::Usage;
use strict;
use warnings;
use autodie;
use vars qw($init_cfg @clusters $settings $action $debug);

#########################################################################
### configure file parser
package config;
sub init {
  # Check the file
  my $file = shift or die 'You did not specify a file name';
  die "File '$file' does not exist"               unless -e $file;
  die "'$file' is a directory, not a file"        unless -f _;
  die "Insufficient permissions to read '$file'"  unless -r _;

  # Slurp in the file
  local $/ = undef;
  open my $fh, '<', $file;
  my $contents = <$fh>;
  close $fh;

  read_string($contents);
}

sub read_string($) {
  # Parse the string
  my $server_type = 'public';
  my $group       = undef;
  my $self        = undef;
  my $counter     = 0;
  my @servers     = ();
  foreach ( split /(?:\015{1,2}\012|\015|\012)/, shift ) {
    $counter++;

    # Handle real a server
    if (/^\s*(\d+\.\d+\.\d+\.\d+)\s*$/) {
      my $cluster_id = '0';
      if ($group =~ /cluster_(\d+)/) {
        $cluster_id = $1;
      } else {
        die 'wrong group name';
      }
      push @servers, { %{$self->{global}{public}},
                       %{$self->{global}{$server_type}},
                       %{$self->{$group}{public}},
                       %{$self->{$group}{$server_type}},
                       'server_type' => $server_type,
                       'ip'          => $_,
                       'cluster_id'  => $cluster_id };
      # add lms if match rootserver
      if ($server_type eq 'rootserver') {
        push @servers, { %{$self->{global}{public}},
                         %{$self->{global}{mergeserver}},
                         %{$self->{$group}{public}},
                         'server_type' => 'listener_mergeserver',
                         'ip'          => $_,
                         'cluster_id'  => $cluster_id };
      }
      next;
    }

    # Handle begin group
    if (/^#\@begin_(.+)$/) {
      if (not $group) {
        $self->{$group = $1} ||= {};
        $server_type = 'public';
      } else {
        # multi begin group line
        return die( "Syntax error at line $counter: '$_'");
      }
      next;
    }

    # Handle end group
    if (/^#\@end_(.+)$/) {
      $group = undef;
      my $cur_group = $1;
      next unless $cur_group =~ /^cluster_(\d+)$/;
      my $cluster = {
                     'rootserver'           => [grep {$_->{server_type} eq 'rootserver'} @servers],
                     'updateserver'         => [grep {$_->{server_type} eq 'updateserver'} @servers],
                     'chunkserver'          => [grep {$_->{server_type} eq 'chunkserver'} @servers],
                     'mergeserver'          => [grep {$_->{server_type} eq 'mergeserver'} @servers],
                     'listener_mergeserver' => [grep {$_->{server_type} eq 'listener_mergeserver'} @servers],
                     'cluster_id'           => $cur_group,
                     'master_cluster_id'    => $self->{global}{public}{master_cluster_id},
                    };
      $cluster = {
                  %$cluster,
                  'vip'      => $cluster->{rootserver}[0]{vip},
                  'rs_port'  => $cluster->{rootserver}[0]{port},
                  'lms_port' => $cluster->{listener_mergeserver}[0]{lms_port}
                 };
      push @main::clusters, new Cluster($cluster);
      @servers = ();

      next;
    }

    # Skip comments and empty lines
    next if /^\s*(?:\#|\;|$)/;

    # Remove inline comments
    s/\s\;\s.+$//g;

    # Handle section headers
    if ( /^\s*\[\s*(.+?)\s*\]\s*$/ ) {
      if ($group) {
        $self->{$group}{$server_type = $1} ||= {};
      } else {
        $self->{$server_type = $1} ||= {};
      }
      next;
    }

    # Handle properties
    if ( /^\s*([^=]+?)\s*=\s*(.*?)\s*$/ ) {
      if ($group eq 'settings') {
        $main::settings->{$1} = $2;
      }

      if ($group) {
        $self->{$group}{$server_type}{$1} = $2;
      } else {
        $self->{$server_type}{$1} = $2;
      }
      next;
    }

    die "Syntax error at line $counter: '$_'";
  }

  $main::init_cfg = $self->{init_config};
  $main::settings = $self->{global}{settings};
}

######################################################################
### common functions
package common;
sub pinfo($) {
  print "[INFO] $_[0]\n";
}

sub perror($) {
  print "[ERROR] $_[0]\n";
}

sub pwarn($) {
  print "[WARN] $_[0]\n";
}

sub pdebug($) {
  $main::debug and print "[DEBUG] $_[0]\n";
}

sub do_ssh($$$) {
  my ($ip, $cmd, $prompt) = @_;
  $cmd = "export LD_LIBRARY_PATH=\$LD_LIBRARY_PATH:/usr/local/lib:/usr/local/lib64:$main::settings->{ob_home}/lib "
    . "&& cd $main::settings->{ob_home} && ulimit -c unlimited && $cmd 2>&1";

  my $ssh_cmd = "ssh admin\@$ip '$cmd'";
  pdebug($ssh_cmd);
  if ($prompt) {
    pinfo($prompt);
    qx($ssh_cmd);
  } else {
    system $ssh_cmd;
  }
}

sub do_server($$) {
  my ($op, $server) = @_;
  my $cmd = "bin/$server";
  if ($op eq 'start') {
    $cmd = "export LD_LIBRARY_PATH=\$LD_LIBRARY_PATH:/usr/local/lib:/usr/local/lib64:/home/admin/oceanbase/lib:./lib "
      . "&& cd /home/admin/oceanbase && $cmd";
    pdebug($cmd);
    system($cmd);
  } elsif ($op eq 'stop') {
    system("killall -u admin -e $cmd");
  } else {
    die "Unknow operate `$op'";
  }
}

sub check_ssh {
  system("ssh -T -o PreferredAuthentications=publickey $_[0] -l admin -o ConnectTimeout=1 true") == 0
    or die "ssh check failed! host: [$_[0]]";
  return $?;
}

sub bootstrap ($$) {
  my ($ip, $port, $role) = @_;
  my $rs_admin = $main::settings->{rs_admin};
  do_ssh($ip, "$rs_admin -r $ip -p $port -t 60000000 boot_strap", "bootstrap...");
}

sub get_mmrs() {
  die 'no clusters' if @main::clusters <= 0;
  my $master_cluster = $main::clusters[0]->{master_cluster_id};
  my @master_ob = grep { $_->{cluster_id} eq $master_cluster } @main::clusters;
  ($master_ob[0]->{vip}, $master_ob[0]->{rs_port});
}

sub get_one_lms() {
  die 'no clusters' if @main::clusters <= 0;
  my $master_cluster = $main::clusters[0]->{master_cluster_id};
  my @master_ob = grep { $_->{cluster_id} eq $master_cluster } @main::clusters;
  ($master_ob[0]->{vip}, $master_ob[0]->{lms_port});
}

sub do_sql_query($) {
  my $sql = shift;
  my ($host, $port) = get_one_lms();
  my ($user, $password) = ('admin', 'admin');
  qx(mysql -h$host -P$port -u$user -p$password -Bs -e "$sql");
}

sub verify_bootstrap {
  for my $cluster (@main::clusters) {
    my $cluster_id = $cluster->{cluster_id};
    $cluster_id =~ s/^cluster_//;
    my $role = $cluster->{master_cluster_id} eq $cluster->{cluster_id} ? 1 : 2;
    my $sql = "SELECT count(1) FROM __all_cluster "
      . "WHERE cluster_id = $cluster_id AND cluster_vip = '$cluster->{vip}' "
        . "AND cluster_port = $cluster->{lms_port} AND cluster_role = $role";
    my $cnt = do_sql_query($sql);
    if ($cnt == 1) {
      pinfo("Verify cluster [$cluster_id] OK!");
    } else {
      # not ok
      perror("__all_cluster table is not legal!");
      return 0;
    }
  }

  for my $cluster (@main::clusters) {
    my @servers = ('rootserver', 'mergeserver', 'chunkserver', 'updateserver');
    while (my ($k, $v) = each %$cluster) {
      my @server_name = grep { $k =~ /^$_$/ } @servers;
      if (@server_name > 0) {
        my $server_bin = shift @server_name;
        map {
          my $sql = "SELECT count(1) FROM __all_server WHERE svr_type='$server_bin'"
            . " AND svr_ip = '$_->{ip}'";
          chomp (my $cnt = do_sql_query($sql));
          if ($cnt == 1) {
            pinfo("Verify server OK! [<$server_bin> $_->{ip}]");
          } else {
            # not ok
            perror("__all_server table is not legal! [<$server_bin> $_->{ip}]");
            return 0;
          }
        } @$v;
      }
    }
  }

  for my $cluster (@main::clusters) {
    my @servers = ('rootserver', 'mergeserver', 'chunkserver', 'updateserver');
    while (my ($k, $v) = each %$cluster) {
      my @server_name = grep { $k =~ /^$_$/ } @servers;
      if (@server_name > 0) {
        my $server_bin = shift @server_name;
        map {
          my $sql = "SELECT count(1) FROM __all_sys_config_stat WHERE svr_type='$server_bin'"
            . " AND svr_ip = '$_->{ip}' AND svr_port = '$_->{port}'";
          chomp (my $cnt = do_sql_query($sql));
          if ($cnt > 1) {
            pinfo("Verify config OK! [<$server_bin> $_->{ip}:$_->{port} cnt:$cnt]");
          } else {
            # not ok
            perror("__all_sys_config_stat table is not legal! [<$server_bin> $_->{ip}]");
            return 0;
          }
        } @$v;
      }
    }
  }
  pinfo("Verify okay.");
  return 1;
}

sub quicktest() {
  my ($lms_ip, $lms_port) = get_one_lms();

  for my $test ("create", "show", "count_distinct", "join_basic", "group_by_1", "sq_from", "ps_complex") {
    print "[TEST] $test\n";
    system("bin/mysqltest --logdir=tests --port=$lms_port --tmpdir=tests --database=test --timer-file=tests --user=admin --host=$lms_ip --result-file=tests/${test}.result --test-file=tests/${test}.test --tail-lines=10 --password=admin --silent");
  }
}

sub run_mysql() {
  my ($host, $port)     = get_one_lms();
  my ($user, $password) = ('admin', 'admin');
  my $cmd               = "mysql -h$host -P$port -u$user -p$password";
  system split / /, $cmd;
}

#######################################################################
### cluster manipulation
package Cluster;

sub new {
  my ($class, $self) = @_;
  bless ($self, $class);
}

sub delete_data($) {
  my $cluster = shift;

  map {
    my $cs = $_;
    my $cmd = "rm -rf /data/{1..$cs->{max_disk_num}}/$cs->{appname}/ $main::settings->{ob_home}/{log,data,run,etc/*.bin}";
    common::do_ssh($cs->{ip}, $cmd, "Delete data of cs:$cs->{ip}");
  } @{$cluster->{chunkserver}};

  map {
    my $cmd = "rm -rf $main::settings->{ob_home}/{log,data,run,etc/*.bin}";
    common::do_ssh($_->{ip}, $cmd, "Delete data of ms:$_->{ip}");
  } (@{$cluster->{mergeserver}}, @{$cluster->{lister_mergeserver}});

  map {
    my $ups = $_;
    my $cmd = "rm -rf /data/{1..$ups->{max_disk_num}}/ups_data/ $ups->{commitlog_dir} $main::settings->{ob_home}/{log,data,run,etc/*.bin}";
    common::do_ssh($ups->{ip}, $cmd, "Delete data of ups:$ups->{ip}");
  } @{$cluster->{updateserver}};

  map {
    my $cmd = "rm -rf $_->{commitlog_dir} $main::settings->{ob_home}/{log,data,run,etc/*.bin}";
    common::do_ssh($_->{ip}, $cmd, "Delete data of rs:$_->{ip}");
  } @{$cluster->{rootserver}};
}

sub stop($) {
  my $self = shift;

  my @servers = ('rootserver', 'mergeserver', 'chunkserver', 'updateserver');
  while (my ($k, $v) = each %$self) {
    my @server_name = grep { $k =~ /$_/ } @servers;
    if (@server_name > 0) {
      my $server_bin = shift @server_name;
      map { common::do_ssh($_->{ip},
                           "killall -u admin -e bin/$server_bin",
                           "stop $_->{server_type} [$_->{ip}]") } @$v;
    }
  }
}

sub status($) {
  my $self = shift;
  my ($absense_any, $found_any) = (0, 0);

  my @servers = ('rootserver', 'mergeserver', 'chunkserver', 'updateserver');
  my %ip_list = ();
  map { $ip_list{$_} = [] } @servers;
  while (my ($k, $v) = each %$self) {
    my @server_name = grep { $k =~ /$_/ } @servers;
    if (@server_name > 0) {
      my $server_bin = shift @server_name;
      map { push @{$ip_list{$server_bin}}, $_->{ip} } @$v;
    }
  }

  common::pinfo("cluster id: [$self->{cluster_id}]");
  while (my ($k, $v) = each %ip_list) {
    common::pinfo("$k:");
    map {
      printf "\t%-16s: ", $_;
      if (common::do_ssh($_, "ps -C $k -o cmd,user,pid | grep admin", "")) {
        print "absense of *$k*\n";
        $absense_any = 1;
      } else {
        $found_any = 1;
      }
    } @$v;
  }
  ($absense_any, $found_any);
}

sub set_obi_role($) {
  my $cluster = shift;
  my ($ip, $port, $role, $rs_admin) = ($cluster->{rootserver}[0]{vip},
                                       $cluster->{rootserver}[0]{port},
                                       $cluster->{cluster_id} eq $cluster->{master_cluster_id} ? "OBI_MASTER" : "OBI_SLAVE",
                                       $main::settings->{rs_admin});
  common::do_ssh($ip, "$rs_admin -r $ip -p $port set_obi_role -o $role", "Set obi role [$cluster->{cluster_id} $role]");
}

sub check_ssh($) {
  my $cluster = shift;
  my @all_servers_type = grep { /server$/ } keys %$cluster;
  for my $server_type (@all_servers_type) {
    map { common::check_ssh($_->{ip}); } @{$cluster->{$server_type}};
  }
}

sub start_rootservers($) {
  my $self = shift;
  my $rootservers = $self->{rootserver};
  map {
    $self->__start_one_rootserver($_);
  } @$rootservers;
}

sub start_chunkservers($) {
  my $self = shift;
  my ($chunkservers, $rs_vip, $rs_port) = ($self->{chunkserver}, $self->{vip}, $self->{rs_port});
  map {
    $self->__start_one_chunkserver($_);
  } @$chunkservers;
}

sub start_mergeservers($) {
  my $self = shift;
  my ($mergeservers, $rs_vip, $rs_port) = ($self->{mergeserver}, $self->{vip}, $self->{rs_port});
  my $cfg = $main::init_cfg->{mergeserver};
  my $init_cfg_str = join ',',map { "$_=$cfg->{$_}" } keys %$cfg;
  for my $ms (@$mergeservers) {
    my $cmd = "bin/mergeserver -r $rs_vip:$rs_port -p $ms->{port} -z $ms->{sql_port} -i $ms->{devname}";
    $cmd .= " -o $init_cfg_str" if $init_cfg_str;
    common::do_ssh($ms->{ip}, $cmd, "Start $ms->{server_type} [$ms->{ip}]");
  }
}

sub start_updateservers($$$) {
  my $self = shift;
  my ($updateservers, $rs_vip, $rs_port) = ($self->{updateserver}, $self->{vip}, $self->{rs_port});
  map {
    $self->__start_one_updateserver($_);
  } @$updateservers;
}

sub start_lms($) {
  my $self = shift;
  my ($lmss, $rs_vip, $rs_port) = ($self->{listener_mergeserver}, $self->{vip}, $self->{rs_port});

  my $init_cfg = $main::init_cfg->{mergeserver};
  my $init_cfg_str = join ',',map { "$_=$init_cfg->{$_}" } keys %$init_cfg;

  for my $lms (@$lmss) {
    my $cmd = "bin/mergeserver -r $rs_vip:$rs_port -p $lms->{port} -z $lms->{lms_port} -i $lms->{devname}";
    $cmd .= " -o $init_cfg_str" if $init_cfg_str;
    common::do_ssh($lms->{ip}, $cmd, "Start $lms->{server_type} [$lms->{ip}]");
  }
}

sub start($$) {
  my ($self, $first) = @_;

  if ($first) {
    $self->start_rootservers();
    $self->start_updateservers();
    $self->start_chunkservers();
    $self->start_mergeservers();
    $self->start_lms();
  } else {
    my @servers = ('rootserver', 'mergeserver', 'chunkserver', 'updateserver');
    while (my ($k, $v) = each %$self) {
      my @server_name = grep { $k =~ /$_/ } @servers;
      if (@server_name > 0) {
        my $server_bin = shift @server_name;
        map { common::do_ssh($_->{ip}, "bin/$server_bin", "Start $_->{server_type} [$_->{ip}]") } @$v;
      }
    }
  }
  $self->set_obi_role();
}

sub bootstrap($) {
  my $self = shift;

  if ($self->{cluster_id} eq $self->{master_cluster_id}) {
    common::bootstrap($self->{vip}, $self->{rs_port});
  }
}

# belows are private functions
sub __start_one_rootserver($$) {
  my ($self, $rs) = @_;
  die "Not rootserver!" unless $rs->{server_type} eq 'rootserver';

  my ($master_rs_vip, $master_rs_port) = common::get_mmrs();
  my $init_cfg = $main::init_cfg->{rootserver};
  my $init_cfg_str = join ',',map { "$_=$init_cfg->{$_}" } keys %$init_cfg;
  my $cmd = '';
  if ($main::action eq 'init') {
    $cmd = "mkdir -p data/rs $rs->{commitlog_dir} && ln -s $rs->{commitlog_dir} data/rs_commitlog"
      . " && echo -e \"[app_name]\nname=$rs->{appname}\nmax_table_id=1500\" >etc/schema.ini && ";
  }
  $cmd .= "bin/rootserver -r $rs->{vip}:$rs->{port} -R $master_rs_vip:$master_rs_port -i $rs->{devname} -C $rs->{cluster_id}";
  $cmd .= " -o $init_cfg_str" if $init_cfg_str;
  common::do_ssh($rs->{ip}, $cmd, "Start $rs->{server_type} [$rs->{ip}]");
}

sub __start_one_chunkserver($$$) {
  my ($self, $cs) = @_;
  my ($rs_vip, $rs_port) = ($self->{vip}, $self->{rs_port});
  die "Not chunkserver!" unless $cs->{server_type} eq 'chunkserver';

  my $init_cfg = $main::init_cfg->{chunkserver};
  my $init_cfg_str = join ',',map { "$_=$init_cfg->{$_}" } keys %$init_cfg;
  my $cmd = 'true';
  if ($main::action eq "init") {
    $cmd = "for ((i=1; i<=$cs->{max_disk_num}; i++)) do mkdir -p /data/\$i/$cs->{appname}/sstable; done"
      . " && mkdir -p data"
        . " && for ((i=1; i<=$cs->{max_disk_num}; i++)) do ln -s -T /data/\$i data/\$i; done";
  }
  $cmd .= "&& bin/chunkserver -r $rs_vip:$rs_port -p $cs->{port} -n $cs->{appname} -i $cs->{devname}";
  $cmd .= " -o $init_cfg_str" if $init_cfg_str;
  common::do_ssh($cs->{ip}, $cmd, , "Start $cs->{server_type} [$cs->{ip}]");
}

sub __start_one_updateserver($) {
  my ($self, $ups) = @_;
  my ($rs_vip, $rs_port) = ($self->{vip}, $self->{rs_port});
  die "Not updateserver!" unless $ups->{server_type} eq 'updateserver';

  my $cfg = $main::init_cfg->{updateserver};
  my $init_cfg_str = join ',',map { "$_=$cfg->{$_}" } keys %$cfg;
  my $cmd = 'true';
  if ($main::action eq "init") {
    $cmd = "mkdir -p data/ups_data/raid{" . join(',', (0 .. ($ups->{max_disk_num} - 1)/ 2)) . "}"
      . " && mkdir -p $ups->{commitlog_dir} && ln -s $ups->{commitlog_dir} data/ups_commitlog";
    map {
      $cmd .= " && mkdir -p /data/$_/ups_data && ln -s -T /data/$_/ups_data data/ups_data/raid" . int(($_ - 1) / 2) . "/store" . ($_ - 1) % 2;
    } (1 .. $ups->{max_disk_num});
  }
  $cmd .= " && bin/updateserver -r $rs_vip:$rs_port -p $ups->{port} -m $ups->{inner_port} -i $ups->{devname}"
    . ($init_cfg_str and " -o $init_cfg_str" or "");

  common::do_ssh($ups->{ip}, $cmd, "Start $ups->{server_type} [$ups->{ip}]");
}

#############################################################################
## start server and cluster
package main;
local *main::pinfo = *common::pinfo;
local *main::pdebug = *common::pdebug;

sub local_op($) {
  $_ = $_[0];
  my ($op, $server) = ();
  my %svr_map = ('cs' => 'chunkserver', 'ms' => 'mergeserver',
                 'ups' => 'updateserver', 'rs' => 'rootserver');

  if (/^(start|stop)_(cs|ms|ups|rs)$/) {
    $op = $1;
    $server = $svr_map{$2};
  } else {
    die "`$_' is an unvalid action!";
  }
  common::do_server($op, $server);
}

sub all_op {
  my $cfg_file = pop @ARGV;
  $action = shift @ARGV;

  my $help = '';
  my $force = '';
  my $cluster_id = '';
  my $result = GetOptions("force"     => \$force,
                          "cluster=s" => \$cluster_id,
                          "debug"     => \$debug) or pod2usage(1);
  pod2usage(1) if (not $action =~ m/^dump$|^init$|^clean$|^stop$|^start$|^check$|^mysql$|^status$/);

  config::init($cfg_file);
  my @cur_clusters = grep { $_->{cluster_id} =~ /cluster_$cluster_id/ } @clusters;

  if ($action eq "dump") {
    print Dumper(@clusters);
    exit(0);
  } elsif ($action eq "clean") {
    if (not $force) {
      $|=1;
      print "Will *DELETE ALL DATA* from servers, sure? [y/N] ";
      read STDIN, my $char, 1;
      exit (0) if $char ne 'y' and $char ne 'Y';
    }

    map { $_->check_ssh() } @cur_clusters;
    map { $_->delete_data() } @cur_clusters;

    exit(0);
  } elsif ($action =~ "start") {
    map { $_->check_ssh() } @cur_clusters;
    map {
      $_->start(0);
    } @cur_clusters;
  } elsif ($action =~ "init") {
    if (not $force) {
      $|=1;
      print "Will *DELETE ALL DATA* from servers, sure? [y/N] ";
      read STDIN, my $char, 1;
      exit (0) if $char ne 'y' and $char ne 'Y';
    }

    map { $_->check_ssh() } @cur_clusters;
    map {
      $_->start(1);
      # will ignore non master cluster
      $_->bootstrap();
    } @cur_clusters;

    pinfo("Bootstrap ok, verifing status...");

    my $verify_ok = 1;
    for (1..6) {
      sleep 10;
      $verify_ok = common::verify_bootstrap() and last;
    }
    die "OceanBase verify failed. " unless $verify_ok;

    ### restart all servers
    map {
      $_->stop();
    } @cur_clusters;

    my $found_any = 0;
    for (1..20) {
      map {
        $found_any = ($_->status())[1];
        pdebug("found_any: $found_any");
        if ($found_any) {
          sleep 3 && next;
        }
      } @cur_clusters;
      last if not $found_any;
    }
    die "Stop all servers not successfully." if $found_any;

    pinfo("Stop all done!");

    map {
      $_->start();
    } @cur_clusters;

    my $absense_any = 0;
    for (1..20) {
      map {
        $absense_any = ($_->status())[0];
        pdebug("absense_any: $absense_any");
        sleep 3 && next if $absense_any;
      } @cur_clusters;
      last if not $absense_any;
    }
    die "Start all servers not successfully." if $absense_any;

    exit(0);
  } elsif ($action eq "stop") {
    if (not $force) {
      $|=1;
      print "Stop all server!! Sure? [y/N] ";
      read STDIN, my $char, 1;
      exit (0) if $char ne 'y' and $char ne 'Y';
    }
    map { $_->check_ssh() } @cur_clusters;
    map { $_->stop() } @cur_clusters;
  } elsif ($action eq 'status') {
    map { $_->check_ssh() } @cur_clusters;
    map { $_->status() } @cur_clusters;
  } elsif ($action eq "check") {
    map { $_->check_ssh() } @cur_clusters;
    common::verify_bootstrap;
    common::quicktest;
  } elsif ($action eq "mysql") {
    common::run_mysql;
  }
}

sub main {
  pod2usage(1) if ((@ARGV < 1) && (-t STDIN));

  local_op($ARGV[0]) if (@ARGV == 1);
  all_op($ARGV[0]) if (@ARGV > 1);
}

main;

__END__

=head1 NAME

    oceanbase.pl - a script to deploy oceanbase clusters.

=head1 SYNOPSIS

=item oceanbase.pl B<Action> [L<Options>] config_file  (1st form)

=item oceanbase.pl B<Local_Action>                     (2nd form)

=back

=begin pod

    +----------------------------------------------------------------------+
    | Normal order using this script:                                      |
    |                                                                      |
    | 1. edit config_file as you like.                                     |
    | 2. run `./oceanbase.pl init config_file' to init all ob cluster.     |
    | 3. run `./oceanbase.pl check config_file' to verify weather ob is ok.|
    +----------------------------------------------------------------------+

=end pod

=head2 ACTION

=item init

Init oceanbase as L<config_file> descripted. It'll create necessary directories and links.

=item check

Check OceanBase instance weather it've inited successfully.

=item start

Only start servers.

=item stop

Stop all servers.

=item dump

Dump configuration read from config_file

=item clean

Clean all data and B<STOP> all server.

=item mysql

Connect to ob with listener ms.

=head2 LOCAL_ACTION

=item start_rs|stop_rs

=item start_cs|stop_cs

=item start_ms|stop_ms

=item start_ups|stop_ups

=head1 OPTIONS

=item B<--cluster,-c> CLUSTER_ID

All actions is for that cluster except for dump.

=item B<--force>

Force run command without asking anything.

=item B<--debug>

Run in debug mode.

=back

=head1 AUTHOR

Yudi Shi - L<fufeng.syd@alipay.com>

=head1 DESCRIPTION

=cut
