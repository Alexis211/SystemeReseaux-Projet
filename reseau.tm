<TeXmacs|1.0.7.21>

<style|generic>

<\body>
  Idée de base : utiliser stdin/stdout pour la communication et avoir un
  machin qui communique à côté. En mode client : faire un socket.connect puis
  faire un dup2 pour que stdio redirige vers le socket. Serveur : appli à
  part.

  <strong|Protocole Manager-Application.>

  <em|Messages application.>

  <\itemize>
    <item>Emit(task, IO?)

    <item>Get(chan-id, cont)

    <item>Put(chan-id, data)

    <item>Request-task(IO?)

    <item>Emit-result
  </itemize>

  <em|Manager application.>

  <\itemize>
    <item>Give-task(task, iO?)

    <item>Give-task-get(data, cont, IO?)
  </itemize>

  <strong|Protocole Manager-Manager.>

  same as manager-application, mostly

  <strong|Protocole Manager-Pool.>

  <\itemize>
    <item>manager serve : connect to pool, send Serve

    <item>manager use : connect to pool, send NeedHelp(task name, my addr)

    <item>pool relay : send NeedHelp(task name, my addr)
  </itemize>

  <strong|Interface.>

  On lance un pool-server sur une machine :

  <verbatim|tulipier$ ./manager --pool>

  Puis on lance plein de travaillers :

  <\verbatim>
    cargo$ ./manager --serve tulipier

    tilleul$ ./manager --serve tulipier

    thuya$ ./manager --serve tulipier
  </verbatim>

  On écrit un code <verbatim|mon_app.ml> qui utilise la bibliothèque
  <verbatim|Kahn_sock.Sock_dist>. On compile, et on s'assure que l'on peut
  appeller le binaire avec <verbatim|./mon_app> sur toutes les machines qui
  sont en mode <verbatim|--serve>. Ensuite, on fait :

  <verbatim|tulipier$ ./manager --run ./mon_app --use tulipier>

  \;
</body>