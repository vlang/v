
/* Description of data base entry for a single host.  */
struct hostent {
     char *h_name;         /* Official name of host.  */
     char **h_aliases;     /* Alias list.  */
     int h_addrtype;       /* Host address type.  */
     int h_length;         /* Length of address.  */
     char **h_addr_list;       /* List of addresses from name server.  */
};
