#ifndef _CinemaOTNode_H_
#define _CinemaOTNode_H_

namespace CINEMA
{
   // Address types (O_ADDRESS | P_TYPE)
#ifndef ADR_UNKNOWN
   enum ADR
   {
      ADR_UNKNOWN = 0,
      ADR_IP      = 1,
      ADR_IPX     = 2,
      ADR_MAC     = 3,
      ADR_HOST    = 4,
      ADR_ATM     = 5,
      ADR_IPV6	= 6
   };
#endif
#define ADDRESS_TYPE_ARRAY(name) std::pair<int, std::string> name [] = { \
   std::make_pair (ADR_IP, "IP"), \
   std::make_pair (ADR_IPX, "IPX"), \
   std::make_pair (ADR_MAC, "MAC"), \
   std::make_pair (ADR_HOST, "Host"), \
   std::make_pair (ADR_ATM, "ATM"), \
   std::make_pair (ADR_IPV6, "IPv6") \
      };
#define ADDRESS_TYPE_MAP(mapname,arrayname) std::map<int, std::string> mapname (arrayname, arrayname + sizeof (arrayname) / sizeof (std::pair<int, std::string>));
   // Subtypes (O_SUBTYPE | P_SUBTYPE)
   enum SBT
   {
      SBT_UNKNOWN        = -1,
      SBT_DEVICE         = 0,
      SBT_PORT           = 1,
      SBT_SLOT           = 2,
      SBT_INTERFACE      = 3,
      SBT_VLAN           = 4,
      SBT_TRUNK          = 5,
      SBT_MEMORY         = 6,
      SBT_PROCESSOR      = 7,
      SBT_POWERSUPPLY    = 8,
      SBT_FAN            = 9,
      SBT_USERDEFINED    = 10,
      SBT_STORAGE        = 11,
      SBT_WLAN           = 12,
      SBT_TUNNEL         = 13,
      SBT_VOICE			 = 14,
      SBT_MEASURING_SECTION = 15,
      SBT_SERVICE        = 16,
      SBT_SW_INSTALLED   = 17,
      SBT_SW_RUNNING     = 18,
      SBT_OPERATING_SYSTEM = 19,
      SBT_PRINTER_QUEUE  = 20,
      SBT_PARTITION      = 21,
      SBT_ACCOUNT        = 22,
      SBT_APPLICATION_MANAGER = 23,
      SBT_EXTERNAL_DEVICE = 24,
      SBT_SESSION        = 25,
      SBT_DOMAIN         = 26,
      SBT_SHARE          = 27,
      SBT_TEMPERATURE    = 28,
      SBT_MPLS_VPN       = 29,
      SBT_EXTERNAL		 = 30,
      SBT_FRAME_RELAY    = 31,
      SBT_STORAGE_DEVICE = 32,
      SBT_VIRTUAL_MACHINE = 33,
      SBT_NETWORK_MANAGER = 34,
      SBT_SENSOR         = 35,
      SBT_CONTRACT       = 36,
      SBT_ARRAY_DEVICE   = 37,
      SBT_WMI_SERVICE    = 38,
      SBT_WMI_STORAGE    = 39,
      SBT_PHYSICAL_MEMORY = 40,
      SBT_SOFTWARE_LICENCE = 41,
      SBT_SAP            = 42,
      SBT_ORACLE			 = 43,
      SBT_MS_SQL_Server	 = 44,
      SBT_JMX				 = 45,
      SBT_E2E				 = 46,
      SBT_FLOPPY_DRIVE	 = 47,
      SBT_OPTICAL_DRIVE	 = 48,
      SBT_DISK_DRIVE     = 49,
      SBT_TAPE_DRIVE     = 50,
      SBT_DISK_PARTITION = 51,
      SBT_VIDEO_ADAPTER  = 52,
      SBT_MONITOR        = 53,
      SBT_SQL            = 54,
      SBT_SAP_RFC        = 55,
      SBT_SECURITY       = 56,
      SBT_USB_DEVICE     = 57,
      SBT_VMWARE_ESX_SERVER = 58,
      SBT_QOS_CLASS      = 59,
      SBT_IPSEC_TUNNEL   = 60,
      SBT_CITRIX_METAFRAME = 61,
      SBT_OPERATING_SYSTEM_DC = 62,
      SBT_FILE_PARSER_DC = 63,
      SBT_CUSTOM_DC      = 64,
      SBT_MS_ADS_DC      = 65,
      SBT_MS_SQL_SERVER_2000_DC = 66,
      SBT_MS_SQL_SERVER_2005_DC = 67,
      SBT_MS_SQL_SERVER_2008_DC = 68,
      SBT_MS_IIS_DC      = 69,
      SBT_NET_SERVICE_DC = 70,
      SBT_ORACLE_DC      = 71,
      SBT_DB2_UDB_DC     = 72,
      SBT_SAP_R3_DC      = 73,
      SBT_SAP_WEB_AS_DC  = 74,
      SBT_SAP_CONTROL    = 75,
      SBT_SAP_COMPONENTS = 76,
      SBT_SUN_SOLARIS    = 77,
      SBT_HP_UX          = 78,
      SBT_IBM_AIX        = 79,
      SBT_LINUX          = 80,
      SBT_SAP_DB_DC      = 81,
      SBT_MS_EXCHANGE_SERVER = 82,

      SBT_SERVERMONITOR    = 366,

      SBT_DEFAULT			 = 2147483647, // INT_MAX
   };


   // Poll types (O_SUBTYPEPARAM | P_PARAMETER)
   enum PARAM
   {
      PARAM_DEVICE		=0,
      PARAM_PORT			=1,
      PARAM_SLOT			=2,
      PARAM_INTERFACE		=3,
      PARAM_VLAN			=4,
      PARAM_SUBSLOT		=5,
      PARAM_POWERSUPPLY	=6,
      PARAM_FAN			=7,
      PARAM_ADDRESS		=8,
      PARAM_TRUNK			=9,
      PARAM_PROCESSOR		=10,
      PARAM_MEMORY		=11,
      PARAM_MODULE		=12,
      PARAM_NETMASK		=13,
      PARAM_IPADDRESS		=14,
      PARAM_ATMADDRESS	=15,
      PARAM_REMOTEPORT	=16,
      PARAM_REMOTESLOT	=17,
      PARAM_REMOTESUBSLOT	=18,
      PARAM_MACADDRESS	=19,
      PARAM_STORAGE		=20,
      PARAM_WLAN			=21,
      PARAM_TUNNEL		=22,
      PARAM_VOICE			=23,
      PARAM_PARTITION		=24,
      PARAM_TYPE			=25,
      PARAM_MEASURINGSECTION	=26,
      PARAM_USER			=27,
      PARAM_PASSWORD		=28,
      PARAM_EXTUSER		=29,
      PARAM_EXTPASSWORD	=30,
      PARAM_LOOP			=31,
      PARAM_DESTIPADDRESS	=32,
      PARAM_CODEC			=33,
      PARAM_ADVANTAGEFACTOR	=34,
      PARAM_SLAMONITORTYPE=35,
      PARAM_OWNER			=36,
      PARAM_TAG			=37,
      PARAM_TOS			=38,
      PARAM_STARTTIME		=39,
      PARAM_TIMETOLIVE	=40,
      PARAM_DESTPORT		=41,
      PARAM_NUMPACKETS	=42,
      PARAM_FREQUENCY		=43,
      PARAM_PACKETSIZE	=44,
      PARAM_DEVICEMEM		=45,
      PARAM_FTPUSER		=46,
      PARAM_FTPPASSWORD	=47,
      PARAM_SERVICE		=48,
      PARAM_ACCOUNT		=49,
      PARAM_OPERATINGSYSTEM	=50,
      PARAM_PRINTER		=51,
      PARAM_SOFTWARE		=52,
      PARAM_APPLICATION	=53,
      PARAM_SESSIONNAME	=54,
      PARAM_SHARE			=55,
      PARAM_DOMAIN		=56,
      PARAM_FILE			=57,
      PARAM_DIRECTORY		=58,
      PARAM_TEMPERATURE	=59,
      PARAM_MPLS			=60,
      PARAM_DLCI			=61,
      PARAM_STORAGEDEVICE	=62,
      PARAM_VIRTUALMACHINE=63,
      PARAM_SENSOR		=64,
      PARAM_CONTRACT		=65,
      PARAM_CONTROLLER	=66,
      PARAM_DATE			=67,
      PARAM_TIME			=68,
      PARAM_DATETIME		=69,
      PARAM_NAME			=70,
      PARAM_VOLUME		=71,
      PARAM_VIDEOADAPTER	=72,
      PARAM_MONITOR		=73,
      PARAM_SECURITY		=74,
      PARAM_CLASS			=75,
      PARAM_PROCESS		=76,
      PARAM_LOCADDRESS	=77,
      PARAM_REMADDRESS	=78,
      PARAM_CITRIX		=79,
      PARAM_DIRECTION		=80,
      PARAM_INDEX			=81,
      PARAM_POLICY		=82,

	  PARAM_ENTITY		=96,
   };


   // Poll types (O_POLL | P_TYPE)
   enum PTP
   {
      PTP_SNMP       = 0,
      PTP_PING       = 1,
      PTP_TCP        = 2,
      PTP_HTTP       = 3,
      PTP_WINNET     = 4,
      PTP_STATISTIC	= 5,
      PTP_SEQUENCE	= 6,
      PTP_PROPERTIES	= 7,
      PTP_EXTERNAL	= 8,
      PTP_CONNECTOR	= 9,
      PTP_BSM			= 10,
      PTP_WMI			= 11,
      PTP_PARSER_STATUS = 12,
      PTP_PARSER_EVENT = 13,
      PTP_INVALID		= 2147483647, // INT_MAX
   };

#define POLL_TYPE_ARRAY(name) std::pair<int, std::string> name [] = { \
   std::make_pair (PTP_SNMP, "Device data"), \
   std::make_pair (PTP_PING, "Ping"), \
   std::make_pair (PTP_TCP, "TCP"), \
   std::make_pair (PTP_HTTP, "HTTP"), \
   std::make_pair (PTP_WINNET, "Winnet"), \
   std::make_pair (PTP_STATISTIC, "Statistic"),\
   std::make_pair (PTP_SEQUENCE, "Sequence"),\
   std::make_pair (PTP_PROPERTIES, "Properties"),\
   std::make_pair (PTP_EXTERNAL, "External status"),\
   std::make_pair (PTP_CONNECTOR, "Connector"),\
   std::make_pair (PTP_BSM, "BCS"),\
   std::make_pair (PTP_WMI, "WMI"),\
   std::make_pair (PTP_PARSER_STATUS, "Parser Status"),\
   std::make_pair (PTP_PARSER_EVENT, "Parser Event")\
      };
#define POLL_TYPE_MAP(mapname,arrayname) std::map<int, std::string> mapname (arrayname, arrayname + sizeof (arrayname) / sizeof (std::pair<int, std::string>));


   enum PARSER_TYPE
   {
      PARSER_TYPE_UNKNOWN = 0,
      PARSER_TYPE_LOCAL = 1,
      PARSER_TYPE_POWERSHELL = 2,
      PARSER_TYPE_FTP = 3,
      PARSER_TYPE_SQL = 4,
      PARSER_TYPE_SSH = 5,
      PARSER_TYPE_REMOTE = 6,
      PARSER_TYPE_HTTP = 7,
      PARSER_TYPE_WINDOWS_EVENT_LOG = 8,
      PARSER_TYPE_TELNET = 9,
   };

   inline const char* ParserTypeToString (int type, const char* pDefault = "Unknown")
   {
      switch (type)
      {
      case PARSER_TYPE_UNKNOWN: return "Unknown";
      case PARSER_TYPE_LOCAL: return "Local";
      case PARSER_TYPE_POWERSHELL: return "PowerShell";
      case PARSER_TYPE_FTP: return "FTP";
      case PARSER_TYPE_SQL: return "SQL";
      case PARSER_TYPE_SSH: return "SSH";
      case PARSER_TYPE_REMOTE: return "Remote";
      case PARSER_TYPE_HTTP: return "HTTP";
      case PARSER_TYPE_WINDOWS_EVENT_LOG: return "Windows Event Log";
      case PARSER_TYPE_TELNET: return "Telnet";
      }
      return pDefault;
   }

   enum PARSER_ENCRYPT
   {
      PARSER_ENCRYPT_NONE = 0,
      PARSER_ENCRYPT_SSL = 1,
      PARSER_ENCRYPT_SSH = 2,
   };

   inline const char* ParserEncryptToString (int encrypt, const char* pDefault = "Unknown")
   {
      switch (encrypt)
      {
      case PARSER_ENCRYPT_NONE: return "None";
      case PARSER_ENCRYPT_SSL: return "SSL";
      case PARSER_ENCRYPT_SSH: return "SSH";
      }
      return pDefault;
   }

   enum PARSER_UPDATESTRATEGY
   {
      PARSER_UPDATESTRATEGY_NONE = 0,
      PARSER_UPDATESTRATEGY_ROLLING_FILES = 1,
      PARSER_UPDATESTRATEGY_CYCLIC_FILE = 2,
   };

   inline const char* ParserUpdateStrategyToString (int strategy, const char* pDefault = "Unknown")
   {
      switch (strategy)
      {
      case PARSER_UPDATESTRATEGY_NONE: return "None";
      case PARSER_UPDATESTRATEGY_ROLLING_FILES: return "Rolling Files";
      case PARSER_UPDATESTRATEGY_CYCLIC_FILE: return "Cyclic File";
      }
      return pDefault;
   }

   enum PARSER_AGGR
   {
      PARSER_AGGR_NONE = 0,
      PARSER_AGGR_COUNT = 1,
      PARSER_AGGR_MIN = 2,
      PARSER_AGGR_MAX = 3,
      PARSER_AGGR_FIRST = 4,
      PARSER_AGGR_LAST = 5,
      PARSER_AGGR_CONCAT = 6,
   };

   inline const char* ParserAggrToString (int aggr, const char* pDefault = "Unknown")
   {
      switch (aggr)
      {
      case PARSER_AGGR_NONE: return "None";
      case PARSER_AGGR_COUNT: return "Count";
      case PARSER_AGGR_MIN: return "Minimum";
      case PARSER_AGGR_MAX: return "Maximum";
      case PARSER_AGGR_FIRST: return "First";
      case PARSER_AGGR_LAST: return "Last";
      case PARSER_AGGR_CONCAT: return "Concatenate";
      }
      return pDefault;
   }

      enum PARSER_FILE_TYPE
   {
      PARSER_FILE_TYPE_PLAIN_TEXT = 0,
      PARSER_FILE_TYPE_BINARY = 1,
   };

   inline const char* ParserFileTypeToString (int type, const char* pDefault = "Unknown")
   {
      switch (type)
      {
      case PARSER_FILE_TYPE_PLAIN_TEXT: return "Plain Text";
      case PARSER_FILE_TYPE_BINARY: return "Binary";
      }
      return pDefault;
   }
   
   enum PARSER_TEXT_ENCODING
   {
      PARSER_TEXT_ENCODING_ASCII = 0,
      PARSER_TEXT_ENCODING_UTF8 = 1,
      PARSER_TEXT_ENCODING_UTF16 = 2,
      PARSER_TEXT_ENCODING_UTF32 = 3,
      PARSER_TEXT_ENCODING_ISO8859_1= 4,
      PARSER_TEXT_ENCODING_ISO8859_2 = 5,
      PARSER_TEXT_ENCODING_WINDOWS1252 = 6,
   };

   inline const char* ParserTextEncodingToString (int type, const char* pDefault = "Unknown")
   {
      switch (type)
      {
      case PARSER_TEXT_ENCODING_ASCII: return "ASCII";
      case PARSER_TEXT_ENCODING_UTF8: return "UTF-8";
      case PARSER_TEXT_ENCODING_UTF16: return "UTF-16";
      case PARSER_TEXT_ENCODING_UTF32: return "UTF-32";
      case PARSER_TEXT_ENCODING_ISO8859_1: return "ISO 8859-1";
      case PARSER_TEXT_ENCODING_ISO8859_2: return "ISO 8859-2";
      case PARSER_TEXT_ENCODING_WINDOWS1252: return "Windows-1252";
      }
      return pDefault;
   }
   
   enum PARSER_REQUEST
   {
      PARSER_REQUEST_PARSING = 1,
      PARSER_REQUEST_DATA = 2,
      PARSER_REQUEST_EVENT = PARSER_REQUEST_DATA | 4,
      PARSER_REQUEST_STATUS = PARSER_REQUEST_DATA | 8,
   };

   // Threshold types (O_THRESHOLD | P_TYPE)
   enum THT
   {
      THT_INVALID = -1,
      THT_INT     = 0,
      THT_STATE   = 1,
      THT_STR     = 2,
      THT_COUNTER = 3,
   };

   //Flag indicating requested check on HTTP response text (is contained/is not contained) (O_HTTPPOLL | P_CONTFLAG)
   enum HTYPE
   {
      HTYPE_CONTAINMASK       = 0x000F,
      HTYPE_CONTAINNNOT       = 0x0000,
      HTYPE_CONTAINS          = 0x0001,
      HTYPE_AVAILABLE         = 0x0002,
   };

   //SSL Protocol (bit) (O_HTTPPOLL | P_PROTOCOL)
   enum HPROT
   {
      HPROT_PCT1 = 3,
      HPROT_SSL2 = 12,
      HPROT_SSL3 = 48,
      HPROT_TLS1 = 192,
      //
      HPROT_ALL = 255
   };

   // Maintenance status values (O_MAINTENANCE | P_STATUS)
   enum MST
   {
      MST_INACTIVE = 0,
      MST_ACTIVE	 =	1,
      MST_UNKNOWN	 = 2,
   };

   // Object types in nodegroup table (O_NODEGROUPNODE | P_OBJECTTYPE)
   enum GROUP_OT
   {
      GROUP_OT_NODE					= 0,	// NMName
      GROUP_OT_SOFTWARE				= 1,	// InvSWInstalledWhat
      GROUP_OT_SUBTYPE				= 3,	// NMSubtype

      GROUP_OT_HCONFIG				= 4,	// HCConfig  (until NM Version 6.0)
      GROUP_OT_POLL					= 4,	// NMPoll    (since NM Version 6.0)

      GROUP_OT_ASSET_COSTCENTRE	= 2,	// InvAsset (CostCentre)

      GROUP_OT_ASSET_LOCATION		= 5,	// InvAsset (Location)
      GROUP_OT_ASSET_DEPARTMENT	= 6,	// InvAsset (Department)
      GROUP_OT_ASSET_CUSTOM		= 7,	// InvAsset (CustomGroups)

      GROUP_OT_COMPANY_COSTCENTRE	= 8,	// ErmCompany (CostCentre)
      GROUP_OT_COMPANY_LOCATION		= 9,	// ErmCompany (Location)
      GROUP_OT_COMPANY_DEPARTMENT	= 10,	// ErmCompany (Department)

      GROUP_OT_USERDATA_COSTCENTRE	= 11,	// ErmUserdata (CostCentre)
      GROUP_OT_USERDATA_LOCATION		= 12,	// ErmUserdata (Location)
      GROUP_OT_USERDATA_DEPARTMENT	= 13,	// ErmUserdata (Department)

      GROUP_OT_ASSET_SYSTEMGROUP		= 14,	// InvAsset (SystemGroup)
      GROUP_OT_ASSET_COMPONENTGROUP	= 15,	// InvAsset (ComponentGroup)
      GROUP_OT_ASSET_COMPONENTTYPE  = 16,	// InvAsset (ComponentType)

      GROUP_OT_MACRO						= 17,	// CLMacro

      GROUP_OT_MO_TEMPLATE				= 18,	// NMMOTemplate
      GROUP_OT_NODE_TEMPLATE			= 19,	// NMNodeTemplate

      GROUP_OT_PROVISIONING			= 20,	// NMProvisioningNode

      GROUP_OT_CLASS_MENU				= 21,	// CLMenu
      GROUP_OT_CUSTOM_MENU				= 22,	// CLUserMenu

      GROUP_OT_ERMGROUP					= 23,  // ErmGroup
      GROUP_OT_MO_TYPE				= 24,  // MO Type groups
   };

   // Parent-GroupIds in group table (O_NODEGROUP | P_PARENT)
   enum GROUP
   {
      GROUP_ROOT           = 0,  // (virtuel) Root

      GROUP_CUSTOM			= -1,	// CustomGroups
      GROUP_COSTCENTRE		= -2,	// CostCentres
      GROUP_FUNCTION			= -3,	// FunctionGroups
      GROUP_HISTORY			= -4,	// HistoryGroups
      GROUP_SOFTWARE			= -5,	// SoftwareGroups

      GROUP_FUNCTION_MO		= -6,	// FunctionGroups for MOs
      GROUP_CUSTOM_MO		= -7,	// CustomGroups for MOs

      GROUP_SITE				= -8,	 // Sites (for Multi-Site-Edition)
      GROUP_LOCATION			= -9,	 // Locations
      GROUP_DEPARTMENT		= -10, // Departments

      GROUP_PING_DEVICE		= -11, // special FunctionGroup
      GROUP_ROUTER			= -13, // special FunctionGroup
      GROUP_SWITCH			= -17, // special FunctionGroup
      GROUP_HUB				= -16, // special FunctionGroup

      GROUP_SYSTEMGROUP		= -28, // SystemGroups (for Service-Center)
      GROUP_COMPONENTGROUP	= -29, // ComponentGroups (for Service-Center)
      GROUP_COMPONENTTYPE	= -30, // ComponentType (for Service-Center)
      GROUP_PROVISIONING	= -31, // Base group for provisioning

      GROUP_BUSINESSVIEW	= -36, // Function Group for Business Views
      GROUP_UNASSIGNED		= -37, // Function Group for imported nodes
      GROUP_FUNCTION_SITE	= -38, // Function Group for Sites

      GROUP_UPLINK			= -40, // special FunctionGroupMO

      GROUP_NEW_NODES		= -76, // special FunctionGroup
      GROUP_SNMP				= -77, // special FunctionGroup
      GROUP_WMI				= -78, // special FunctionGroup
      GROUP_WBEM				= -79, // special FunctionGroup

      GROUP_AUTHENTICATION	= -84, // Base group for authentication
      GROUP_PROTOCOL       = -99, // Base group for protocols

      GROUP_BV_MO_ELEMENT		= -106, // special FunctionGroupMO containing MOs used in BVs
      GROUP_NET_MO_ELEMENT 	= -107, // special FunctionGroupMO containing MOs used in network objects
      GROUP_BV_NODE_ELEMENT	= -108, // special FunctionGroup containing Nodes used in BVs

      GROUP_SERVICETREE		= -110, // Base group for Service Tree
      GROUP_VIRTUAL_SITES		= -135, // Base group for virtual sites

      GROUP_MO_TYPES			= -136, // Base group for Managed Object types
      GROUP_MO_CUSTOM_TYPES	= -137, // Base group for custom Managed Object types

      GROUP_SYSTEM			= -207, // Function group for system objects
      GROUP_MONITORED_SERVERS = -222, // Base group for 3rdparty Server Monitor (Jan 2011)

      GROUP_DASHBOARDS		= -436, // Base group for Dashboards
      GROUP_DASHBOARD		= -139, // Default group for Dashboards
      GROUP_DEFAULT_DASHBOARD = -624, // Group for Default-Dashboards

      GROUP_SERVER_MONITOR = -634, // Base group for Server Monitor Groups
      GROUP_SERVER_MONITOR_DEVICE = -635, // Function group for Server Monitors


      //attention:  also consider   NODE_GROUP_ARRAY  or  MO_GROUP_ARRAY
   };

   enum OldGroupNotation //only for downward-compatibility
   {
      GROUP_ROOT_CUSTOM				= GROUP_CUSTOM,
      GROUP_ROOT_COSTCENTRE		= GROUP_COSTCENTRE,
      GROUP_ROOT_FUNCTION			= GROUP_FUNCTION,
      GROUP_ROOT_HISTORY			= GROUP_HISTORY,
      GROUP_ROOT_SOFTWARE			= GROUP_SOFTWARE,

      GROUP_ROOT_SITE				= GROUP_SITE,
      GROUP_ROOT_LOCATION			= GROUP_LOCATION,
      GROUP_ROOT_DEPARTMENT		= GROUP_DEPARTMENT,

      GROUP_ROOT_SYSTEMGROUP		= GROUP_SYSTEMGROUP,
      GROUP_ROOT_COMPONENTGROUP	= GROUP_COMPONENTGROUP,
      GROUP_ROOT_COMPONENTTYPE	= GROUP_COMPONENTTYPE,
      GROUP_ROOT_PROVISIONING		= GROUP_PROVISIONING,

      GROUP_ROOT_VIRTUAL_SITES    = GROUP_VIRTUAL_SITES,

      FUNCTION_GROUP_ROUTER      = GROUP_ROUTER,
      FUNCTION_GROUP_SWITCH      = GROUP_SWITCH,
      FUNCTION_GROUP_HUB         = GROUP_HUB,
   };

#define NODE_GROUP_ARRAY(name) int name [] = { \
   GROUP_CUSTOM, \
   GROUP_COSTCENTRE, \
   GROUP_FUNCTION, \
   GROUP_SITE, \
   GROUP_LOCATION, \
   GROUP_DEPARTMENT, \
   GROUP_SYSTEMGROUP, \
   GROUP_PROVISIONING, \
   GROUP_BUSINESSVIEW, \
   GROUP_AUTHENTICATION, \
   GROUP_PROTOCOL, \
   GROUP_SERVICETREE, \
   GROUP_VIRTUAL_SITES, \
   GROUP_SYSTEM, \
   GROUP_MONITORED_SERVERS, \
   GROUP_SERVER_MONITOR, \
   GROUP_DASHBOARDS \
      };
#define NODE_GROUP_SET(setname,arrayname) std::set<int> setname (arrayname, arrayname + sizeof (arrayname) / sizeof (int));

#define MO_GROUP_ARRAY(name) int name [] = { \
   GROUP_FUNCTION_MO, \
   GROUP_CUSTOM_MO \
      };
#define MO_GROUP_SET(setname,arrayname) std::set<int> setname (arrayname, arrayname + sizeof (arrayname) / sizeof (int));

#define POLL_GROUP_ARRAY(name) int name [] = { \
   GROUP_HISTORY \
      };
#define POLL_GROUP_SET(setname,arrayname) std::set<int> setname (arrayname, arrayname + sizeof (arrayname) / sizeof (int));


   // Data source (creator) specification for node objects (O_NODE | P_DATASOURCE)
   enum NDS
   {
      NDS_UNKNOWN						= 0,
      NDS_AGENT						= 1,
      NDS_DISCOVERY					= -2,
      NDS_DC							= 2,
      NDS_SNMP							= 3,
      NDS_MANUAL						= 4,
   };
#define NDS_TYPE_ARRAY(name) std::pair<int, std::string> name [] = { \
   std::make_pair (NDS_AGENT, "Agent"), \
   std::make_pair (NDS_DISCOVERY, "Discovery"), \
   std::make_pair (NDS_DC, "DC"), \
   std::make_pair (NDS_SNMP, "SNMP"), \
   std::make_pair (NDS_MANUAL, "Manual") \
      };
#define NDS_TYPE_MAP(mapname,arrayname) std::map<int, std::string> mapname (arrayname, arrayname + sizeof (arrayname) / sizeof (std::pair<int, std::string>));

   // Poll on demand (O_NODE | P_POLL_ON_DEMAND)
   enum POD
   {
      POD_NEVER						= 0,
      POD_ALWAYS						= 1,
      POD_ON_DEMAND					= 2,
   };
#define POD_TYPE_ARRAY(name) std::pair<int, std::string> name [] = { \
   std::make_pair (POD_NEVER, "Never"), \
   std::make_pair (POD_ALWAYS, "Always"), \
   std::make_pair (POD_ON_DEMAND, "On demand") \
      };
#define POD_TYPE_MAP(mapname,arrayname) std::map<int, std::string> mapname (arrayname, arrayname + sizeof (arrayname) / sizeof (std::pair<int, std::string>));

   // Effective Poll on demand (O_NODE | P_EFF_POLL_ON_DEMAND (virtual only)
   enum EFF_POD
   {
      EFF_POD_NOT_RELEVANT			= 0,
      EFF_POD_ENABLED				= 1,
      EFF_POD_DISABLED				= 2,
   };
#define EFF_POD_TYPE_ARRAY(name) std::pair<int, std::string> name [] = { \
   std::make_pair (EFF_POD_NOT_RELEVANT, ""), \
   std::make_pair (EFF_POD_ENABLED, "Enabled"), \
   std::make_pair (EFF_POD_DISABLED, "Disabled") \
      };
#define EFF_POD_TYPE_MAP(mapname,arrayname) std::map<int, std::string> mapname (arrayname, arrayname + sizeof (arrayname) / sizeof (std::pair<int, std::string>));

   // Command line interface type (O_NODE | P_CLITYPE)
   enum CLI
   {
      CLI_AUTO						   = 0,
      CLI_SSH							= 1,
      CLI_TELNET						= 2,
   };
#define CLI_TYPE_ARRAY(name) std::pair<int, std::string> name [] = { \
   std::make_pair (CLI_AUTO, "Auto"), \
   std::make_pair (CLI_SSH, "SSH"), \
   std::make_pair (CLI_TELNET, "Telnet") \
      };
#define CLI_TYPE_MAP(mapname,arrayname) std::map<int, std::string> mapname (arrayname, arrayname + sizeof (arrayname) / sizeof (std::pair<int, std::string>));


   // protocol version (O_SNMPPOLL | P_VERSION) / authentication type (O_AUTHENTICATION | P_TYPE )
   enum EAuthType //{SNMPV1, SNMPV2, SNMPV3};
   {
	  PV_SNMPV1    = 1,
      PV_SNMPV2    = 2,
      //PV_SNMPM2M = 3,
      PV_SNMPV3    = 4,
      PV_HTTPS     = 5,
      PV_FTP       = 6,
      PV_TELNET    = 7,
      PV_WINNET    = 8,
      PV_SSH		 = 9,
      PV_AUTOCLI	 = 10,
      PV_WMI	    = 11,
      PV_WBEM	    = 12,
      PV_AM		    = 13,
      PV_ESX		 = 14,
      PV_JMX		 = 15,
      PV_E2E		 = 16,
      PV_MS_SQL	 = 17,
	  PV_ORACLE	 = 18,
      PV_SAP_RFC	 = 19,
      PV_SAP_CCMS	 = 20,
      PV_SQL		 = 21,
      PV_SAP_GMP = 22,
      PV_SAP_LMDB = 23,
      PV_SAP_BYD = 24,
      PV_SAP_BCP = 25,
      PV_LDAP 	= 26,
      PV_XML 		= 27,
      PV_MS_EXCH2010= 28,
      PV_SAP_CTRL = 29,
      PV_SAP_JMX = 30,
      PV_CISCO_UCM= 31,
      PV_SAP= 32,
      PV_CROSSGATE = 33,
      PV_DB2          = 34,
      PV_LOGPARSER    = 35,
      PV_SERVICENOW   = 36,
      PV_MS_HYPERV	= 37,
      PV_SAP_MAXDB	= 38,
      PV_REMOTE_USER	= 39,
      PV_MYSQL = 40,
      PV_SMIS = 43,
      PV_SCRIPT =44,
      PV_CITRIX_XEN =45,
      PV_QOS =46,
	  PV_IMPORT_SERVER =47,
	  PV_SAP_HANA =48,
   };
#define AUTH_TYPE_ARRAY(name) std::pair<int, std::string> name [] = {\
   make_pair(PV_SNMPV1, "SNMPv1"),\
   make_pair(PV_SNMPV2, "SNMPv2"),\
   make_pair(PV_SNMPV3, "SNMPv3"),\
   make_pair(PV_HTTPS, "HTTPS"),\
   make_pair(PV_FTP, "FTP"),\
   make_pair(PV_TELNET, "Telnet"),\
   make_pair(PV_WINNET, "Winnet"),\
   make_pair(PV_SSH, "SSH"),\
   make_pair(PV_AUTOCLI, "Auto CLI"),\
   make_pair(PV_WMI, "WMI"),\
   make_pair(PV_WBEM, "WBEM"),\
   make_pair(PV_AM, "AM"),\
   make_pair(PV_ESX, "VMware vSphere"),\
   make_pair(PV_JMX, "JMX"),\
   make_pair(PV_E2E, "E2E"),\
   make_pair(PV_MS_SQL, "MS SQL"),\
   make_pair(PV_ORACLE, "Oracle"),\
   make_pair(PV_SAP_RFC, "SAP RFC"),\
   make_pair(PV_SAP_CCMS, "SAP CCMS"),\
   make_pair(PV_SQL, "SQL"),\
   make_pair(PV_SAP_GMP, "SAP GMP"),\
   make_pair(PV_SAP_LMDB, "SAP LMDB"),\
   make_pair(PV_SAP_BYD, "SAP BYD"),\
   make_pair(PV_SAP_BCP, "SAP BCP"),\
   make_pair(PV_LDAP, "ADS"),\
   make_pair(PV_MS_EXCH2010, "MS Exchange 2010"),\
   make_pair(PV_SAP_CTRL, "SAP CTRL"),\
   make_pair(PV_SAP_JMX, "SAP JMX"),\
   make_pair(PV_SAP, "SAP System"),\
   make_pair(PV_CISCO_UCM, "CISCO UCM"),\
   make_pair(PV_CROSSGATE, "Crossgate"),\
   make_pair(PV_XML, "XML"),\
   make_pair(PV_DB2, "DB2"),\
   make_pair(PV_LOGPARSER, "Logparser"),\
   make_pair(PV_SERVICENOW, "ServiceNow"),\
   make_pair(PV_MS_HYPERV, "MS Hyper-V"),\
   make_pair(PV_SAP_MAXDB, "SAP MaxDB"),\
   make_pair(PV_REMOTE_USER, "PowerShell"),\
   make_pair(PV_MYSQL, "MySQL"),\
   make_pair(PV_SMIS, "SMI-S Storage"), \
   make_pair(PV_SCRIPT, "Script"), \
   make_pair(PV_CITRIX_XEN, "Citrix XenServer"), \
   make_pair(PV_QOS, "Quality of Service"), \
   make_pair(PV_IMPORT_SERVER, "Import Server"), \
   make_pair(PV_SAP_HANA, "SAP HANA"), \
      };
#define AUTH_TYPE_MAP(mapname,arrayname) std::map<int, std::string> mapname (arrayname, arrayname + sizeof (arrayname) / sizeof (std::pair<int, std::string>));


   // security model for authentication set (O_AUTHENTICATION | P_SECURITYMODEL)
   enum ESecurityModel //{NoSecurityModel, USM};
   {
	  SM_NO  = 0,
      SM_USM = 1
   };

   // security type for authentication set (O_AUTHENTICATION | P_SECURITYTYPE)
   enum ESecurityType //{NoSecurity, AuthNoPriv, AuthPriv};
   {
      ST_NOSECURITY = 0,
      ST_AUTHNOPRIV = 1,
      ST_AUTHPRIV   = 2
   };

   // WBEM Protocol Type (O_AUTHENTICATION | P_SECURITYTYPE)
   enum EWBEMProtocolType //{Http, Https};
   {
      WPT_HTTP = 0,
      WPT_HTTPS = 1
   };

   // authentication protocol for snmp polling (O_SNMPPROFILE | P_AUTHPROTOCOL)
   enum EAuthProtocol //{NoAuthentication, HMAC_MD5, HMAC_SHA};
   {
      AP_NONE     = 0,
      AP_HMAC_MD5 = 1,
      AP_HMAC_SHA = 2,
      AP_USER_ONLY = 3,
      AP_PASS_ONLY = 4,
      AP_USER_PASS = 5
   };

   // privacy protocol for snmp polling (O_AUTENTICATION | P_PRIVPROTOCOL)
   enum EPrivProtocol //{NoPrivacy, CBS_DES};
   {
      PP_NONE    = 0,
      PP_CBC_DES = 1,
      PP_CFB_AES128 = 2,
      PP_USER_ONLY = 3,
      PP_PASS_ONLY = 4,
      PP_USER_PASS = 5
   };

   // objecttype for (O_MIBACCESS | P_OBJECTTYPE)
   enum MA_OT //EMibAccessObjectType
   {
      MA_OT_NODE  = 0,
      MA_OT_GROUP = 1,
      MA_OT_CLASS = 2,
      MA_OT_POLL = 3,
   };

   // type (combinations possible) for (O_MIBACCESS | P_TYPE)
   enum MAT //EMibAccessType
   {
      MAT_GET = 1,
      MAT_SET = 2,
      MAT_TRAP = 4
   };


   // pollingtype (O_POLL|P_POLLING_TYPE)
   enum POLLING_TYPE
   {
      PT_STATUS_POLLING   = 1,
      PT_HISTORY_POLLING  = 2,
      PT_EXTERNAL         = 3,
      PT_EXTERNAL_HISTORY = 4,
      PT_SERVERMONITOR    = 5,

      PT_STATUS_TEMPLATE  = 10,
      PT_HISTORY_TEMPLATE = 20,
   };
#define POLLING_TYPE_ARRAY(name) std::pair<int, std::string> name [] = {\
   std::make_pair(PT_STATUS_POLLING, "Status polling"),\
   std::make_pair(PT_HISTORY_POLLING, "History polling"),\
   std::make_pair(PT_EXTERNAL, "External"),\
   std::make_pair(PT_EXTERNAL_HISTORY, "External History"),\
   std::make_pair(PT_SERVERMONITOR, "Mega Farm Monitor"),\
   std::make_pair(PT_STATUS_TEMPLATE, "Template Status polling"),\
   std::make_pair(PT_HISTORY_TEMPLATE, "Template History polling"),\
      };
#define POLLING_TYPE_MAP(mapname,arrayname) std::map<int, std::string> mapname (arrayname, arrayname + sizeof (arrayname) / sizeof (std::pair<int, std::string>));

   // pollingstatus (O_POLL|P_POLLING_STATUS)
   enum POLLING_STATUS
   {
      PS_UNKNOWN	= 0,
      PS_ACTIVE	= 1,
      PS_STOPPED	= 2,
      PS_SCHEDULED= 3,
      PS_DELETED	= 4,
      PS_SUSPENDED= 5,
      PS_WAITING	= 6,
   };
#define POLLING_STATUS_ARRAY(name) std::pair<int, std::string> name [] = { \
   std::make_pair (PS_UNKNOWN, "Unknown"), \
   std::make_pair (PS_ACTIVE, "Active"), \
   std::make_pair (PS_STOPPED, "Stopped"), \
   std::make_pair (PS_SCHEDULED, "Exceeded"), \
   std::make_pair (PS_DELETED, "Deleted"), \
   std::make_pair (PS_SUSPENDED, "Suspended"), \
   std::make_pair (PS_WAITING, "Waiting"), \
      };
#define POLLING_STATUS_MAP(mapname,arrayname) std::map<int, std::string> mapname (arrayname, arrayname + sizeof (arrayname) / sizeof (std::pair<int, std::string>));


   // responsetime evaluation type (O_POLL_60|P_EVALUATION)
   enum EVAL_TYPE //EVALUATION_TYPE
   {
      EVAL_TYPE_RAW   	= 0,
      EVAL_TYPE_MIN	= 1,
      EVAL_TYPE_MAX	= 2,
      EVAL_TYPE_AVG	= 3,
      EVAL_TYPE_LAST	= 4,
      EVAL_TYPE_SUM	= 5
   };
#define EVALUATION_TYPE_ARRAY(name) std::pair<int, std::string> name [] = {\
   std::make_pair(EVAL_TYPE_RAW, "Raw"),\
   std::make_pair(EVAL_TYPE_MIN, "Min"),\
   std::make_pair(EVAL_TYPE_MAX, "Max"),\
   std::make_pair(EVAL_TYPE_AVG, "AVG"),\
   std::make_pair(EVAL_TYPE_LAST, "Last"),\
   std::make_pair(EVAL_TYPE_SUM, "Sum")\
      };
#define EVALUATION_TYPE_MAP(mapname,arrayname)std::map<int, std::string> mapname (arrayname, arrayname + sizeof (arrayname) / sizeof (std::pair<int, std::string>));


   enum AVAILMODE
   {
      AVAILMODE_NO = 0,
      AVAILMODE_CALC = 1,
      AVAILMODE_CORR = 2,
      AVAILMODE_EXTERNAL = 3,
   };
   inline const char* AvailModeToString (int mode, const char* pDefault = "Unknown")
   {
      switch (mode)
      {
      case AVAILMODE_NO: return "None";
      case AVAILMODE_CALC: return "Calculated";
      case AVAILMODE_CORR: return "Correlated";
      case AVAILMODE_EXTERNAL: return "External";
      }
      return pDefault;
   }


   enum COT //_CORRELATION_OBJECT_TYPE  //O_CORRELATION.P_OBJECTTYPE
   {
      COT_A       = 1,
      COT_B       = 2,
      COT_TRUE    = 3,
      COT_FALSE   = 4,
      COT_ELEMENT = 5,

      COT_TEMPLATE = 9,
      //the following ones are also used for O_CORR_OBJECT.P_OBJECTTYPE
      COT_POLL    = 10, //O_POLL
      COT_MO      = 11, //O_SUBTYPE
      COT_NODE    = 12, //O_NODE (Node bzw. Grouping Element)
      COT_MO_FILTER    = 13, //O_MO_TEMPLATE (Managed Object filter object based on managed object template entry)
      COT_NODE_FILTER  = 14, //O_NODE_TEMPLATE (Node filter object based on node template entry)
   };


   enum CORR //_CORRELATION_TYPE  //O_CORRELATION.P_TYPE
   {
      CORR_NONE     = 0, //not defined (undefiniert)

      CORR_RULE     = 1, //use SubObject O_CORR_RULE
      CORR_FUNCTION = 2, //use SubObject O_CORR_FUNCTION
      CORR_VALUE    = 3, //use SubObject O_CORR_VALUE
      CORR_OBJECT   = 4, //use SubObject O_CORR_OBJECT
   };


   enum CR_OP //_CORR_RULE_OPERATOR  //O_CORR_RULE.P_OPERATOR
   {
      CR_OP_EQUAL        = 1, //==
      CR_OP_NOTEQUAL     = 2, //!=
      CR_OP_LESS         = 3, //<
      CR_OP_LESSEQUAL    = 4, //<=
      CR_OP_GREATER      = 5, //>
      CR_OP_GREATEREQUAL = 6, //>=
   };


   enum CF_OP //_CORR_FUNCTION_OPERATION  //O_CORR_FUNCTION.P_OPERATION
   {
      CF_OP_BEST    = 1,
      CF_OP_WORST   = 2,
      CF_OP_AVERAGE = 3,
   };


   enum CFT //_CORR_FUNCTION_TYPE  //O_CORR_FUNCTION.P_TYPE
   {
      CFT_ELEMENTS     = 0, //Objects are defined in the SubVector P_ELEMENTS

      CFT_SUBORDINATE  = 1, //use all directly subordinate Objects (untergeordnete Objekte)
      CFT_STATUS_POLLS = 2, //use only O_POLL-Entries with the Node-Status-Flag
      CFT_ALL_POLLS    = 3,
      CFT_ALL_MO       = 4,
      CFT_ALL_NODES    = 5,
      CFT_FILTER		 = 6,


      CFT_OWN = 255 //corr-thread internal only
   };


   enum ADDRSTAT //_ADDRESS_ADDRESS_STATUS  //O_ADDRESS_ADDRESS.P_STATUS
   {
      ADDRSTAT_FREE     = 0,
      ADDRSTAT_OCCUPIED = 1,
      ADDRSTAT_RESERVED = 2,
   };


   enum ADDRTYPE //_ADDRESS_ADDRESS_TYPE     //O_ADDRESS_ADDRESS.P_TYPE
   {
      ADDRTYPE_NETWORK   = 0,
      ADDRTYPE_BROADCAST = 1,
      ADDRTYPE_DEVICE    = 2,
   };


   enum RESERV //_ADDRESS_RESERVATION_TYPE  //O_ADDRESS_RESERVATION.P_TYPE
   {
      RESERV_SINGLE_ADDRESS = 0,
      RESERV_SUBNET         = 1,
   };

   enum GE_OT //_GE_OBJECT_TYPE //O_GE_ELEMENTS.P_OBJECTTYPE
   {
      GE_OT_NODE = 0, // Node or Grouping Element
      GE_OT_SUBTYPE = 1,
      GE_OT_GROUP = 2,
   };

   enum PEXEC //_POLL_EXEC_PARM		// command_id for SONMPoll::exec()
   {
      PEXEC_REPLACE	= 0,
      PEXEC_DELETE	= 1,
      PEXEC_SET			=	2
   };

   enum STEXEC
   {
      STEXEC_UPDATE_INV = 1,
   };

   enum SRC //_SUBTYPE_PROP_SOURCE //P_SOURCE for O_SUBTYPE, O_SUBTYPE_PROP and O_MOD_SUBTYPE_PROP
   {
      SRC_ALL = -1,
      SRC_SNMP = 0,
      SRC_MANUAL_NOT_OVERWRITEABLE = 1,
      SRC_MANUAL = 2,
      SRC_WMI = 3,
      SRC_IMPORT = 4,
      SRC_TARGET_VALUE = 5,
      SRC_XML = 6,
      SRC_WBEM = 7,
      SRC_BSM = 8,
      SRC_FILTER = 9,
      SRC_SERVERMONITOR = 10,
      SRC_DISCOVERY = 11,
      SRC_SYNC_GE = 12,
      SRC_PROPERTY_OVERRIDE = 13,
      SRC_LDAP = 14,

      SRC_CUSTOM_START = 100,
      
      // !!! HINWEIS: Violeta Marksteiner bei neuen Sourcen informieren!!!
   };
#define PROP_SOURCE_ARRAY(name) std::pair<int, std::string> name [] = { \
   std::make_pair (SRC_ALL, "All"), \
   std::make_pair (SRC_SNMP, "SNMP"), \
   std::make_pair (SRC_MANUAL_NOT_OVERWRITEABLE, "Manual (fixed)"), \
   std::make_pair (SRC_MANUAL, "Manual"), \
   std::make_pair (SRC_WMI, "WMI"), \
   std::make_pair (SRC_IMPORT, "Import"), \
   std::make_pair (SRC_TARGET_VALUE, "Target"), \
   std::make_pair (SRC_XML, "XML"), \
   std::make_pair (SRC_WBEM, "WBEM"), \
   std::make_pair (SRC_BSM, "BCS"), \
   std::make_pair (SRC_FILTER, "Filter"), \
   std::make_pair (SRC_SERVERMONITOR, "MFM"), \
   std::make_pair (SRC_DISCOVERY, "Discovery"), \
   std::make_pair (SRC_SYNC_GE, "SyncModule-GroupElement"), \
   std::make_pair (SRC_PROPERTY_OVERRIDE, "Property override"), \
   std::make_pair (SRC_LDAP, "LDAP"), \
      };
#define PROP_SOURCE_MAP(mapname,arrayname) std::map<int, std::string> mapname (arrayname, arrayname + sizeof (arrayname) / sizeof (std::pair<int, std::string>));

   enum SPS //_SUBTYPE_PROP_STATUS //P_STATUS for O_SUBTYPE_PROP and O_MOD_SUBTYPE_PROP
   {
      SPS_DETECTED = 0,
      SPS_CHANGED = 1,
      SPS_DELETED = 2,
      SPS_AIM = 3,
      SPS_FULFILLED = 4,
   };
#define PROP_STATUS_ARRAY(name) std::pair<int, std::string> name [] = { \
   std::make_pair (SPS_DETECTED, "Detected"), \
   std::make_pair (SPS_CHANGED, "Changed"), \
   std::make_pair (SPS_DELETED, "Deleted"), \
   std::make_pair (SPS_AIM, "Aim"), \
   std::make_pair (SPS_FULFILLED, "Fulfilled") \
      };
#define PROP_STATUS_MAP(mapname,arrayname) std::map<int, std::string> mapname (arrayname, arrayname + sizeof (arrayname) / sizeof (std::pair<int, std::string>));

   enum ModCmdbExec
   {
      MCEXEC_PREPARE = 0,
      MCEXEC_CREATE = 1,
      MCEXEC_FILTEROT = 2,
      MCEXEC_GETCHANGEOT = 3,
   };

   enum NodeImageType //P_TYPE for O_NODEIMAGE
   {
      ITP_BMP			=0,
      ITP_JPG        =1,
      ITP_PNG			=2,
      ITP_EMF			=3,
      ITP_WMF			=4,
      ITP_GIF			=5,
      ITP_TIFF		=6,
      ITP_ICO			=7
   };
#define IMG_TYPE_ARRAY(name) std::pair<int, std::string> name [] = { \
   std::make_pair (ITP_BMP, "BMP"), \
   std::make_pair (ITP_JPG, "JPG"), \
   std::make_pair (ITP_PNG, "PNG"), \
   std::make_pair (ITP_EMF, "EMF"), \
   std::make_pair (ITP_WMF, "WMF"), \
   std::make_pair (ITP_GIF, "GIF"), \
   std::make_pair (ITP_TIFF, "TIF"), \
   std::make_pair (ITP_ICO, "ICO") \
      };
#define IMG_TYPE_MAP(mapname,arrayname) std::map<int, std::string> mapname (arrayname, arrayname + sizeof (arrayname) / sizeof (std::pair<int, std::string>));

   enum OPERATOR //ThresholdOperatorType //O_THRESHOLD|P_OPERATION
   {
      OPERATOR_GREATER		= 0,
      OPERATOR_GREATER_EQUAL	= 1,
      OPERATOR_LESS			= 2,
      OPERATOR_LESS_EQUAL		= 3,
      OPERATOR_EQUAL			= 4,
      OPERATOR_UNEQUAL		= 5,
      OPERATOR_LIKE			= 6,
      OPERATOR_NOT_LIKE		= 7
   };

   enum TPT //Template type (O_NODE_TEMPLATE, O_MO_TEMPLATE | P_TYPE)
   {
      TPT_TEMPLATE = 1,
      TPT_FILTER = 2,
   };

   enum TO //TemplateObjecttype (O_TEMPLATEREF|P_OBJECTTYPE) and TemplateType (O_TEMPLATEREF|P_TEMPLATE_TYPE)
   {
      TO_CLASS = 1,
      TO_GROUP = 2,
      TO_NODE = 3,
      TO_MO = 4,
      TO_NODE_TEMPLATE = 30,
      TO_MO_TEMPLATE = 40,
      TO_POLL_TEMPLATE = 50,
      TO_CORR_TEMPLATE = 100,
      TO_SM_TEMPLATE = 110,
   };


   enum PTS //ProvisioningTask-Status   O_PROVISIONING_TASK|P_STATUS
   {
      PTS_ADDED = 0, //angelegt
      PTS_CHANGED = 1, //geändert
      PTS_APPROVED = 2, //genehmigt
      PTS_FULFILLED = 3, //durchgeführt
      PTS_DECLINED = 4, // abgelehnt
      PTS_TEST = 5,
      PTS_REVIEW = 6,
      PTS_CLOSED = 7,
      PTS_WAIT = 8,
      PTS_ACTIVE = 9
   };
#define PTS_ARRAY(name) std::pair<int, std::string> name [] = { \
   std::make_pair (PTS_ADDED, "Added"), \
   std::make_pair (PTS_CHANGED, "Changed"), \
   std::make_pair (PTS_APPROVED, "Approved"), \
   std::make_pair (PTS_FULFILLED, "Fulfilled"), \
   std::make_pair (PTS_DECLINED, "Declined"), \
   std::make_pair (PTS_TEST, "Testing"), \
   std::make_pair (PTS_REVIEW, "Review"), \
   std::make_pair (PTS_CLOSED, "Closed"), \
   std::make_pair (PTS_WAIT, "Waiting"), \
   std::make_pair (PTS_ACTIVE, "Active") \
      };
#define PTS_MAP(mapname,arrayname) std::map<int, std::string> mapname (arrayname, arrayname + sizeof (arrayname) / sizeof (std::pair<int, std::string>));


   enum PTIT //ProvisioningTaskitem-Type   O_PROVISIONING_TASKITEM|P_TYPE
   {
      PTIT_SCRIPT = 0,
      PTIT_MACRO = 1,
   };
#define PTIT_ARRAY(name) std::pair<int, std::string> name [] = { \
   std::make_pair (PTIT_SCRIPT, "Script"), \
   std::make_pair (PTIT_MACRO, "Macro") \
      };
#define PTIT_MAP(mapname,arrayname) std::map<int, std::string> mapname (arrayname, arrayname + sizeof (arrayname) / sizeof (std::pair<int, std::string>));


   enum PTIO_OT //ProvisioningTaskitemObject-ObjectType   O_PROVISIONING_TASKITEM_OBJECT|P_OBJECTTYPE
   {
      PTIO_OT_CLASS = 0,
      PTIO_OT_GROUP = 1,
      PTIO_OT_NODE  = 2,
   };
#define PTIO_OT_ARRAY(name) std::pair<int, std::string> name [] = { \
   make_pair (PTIO_OT_CLASS, "Class"), \
   make_pair (PTIO_OT_GROUP, "Group"), \
   make_pair (PTIO_OT_NODE, "Node") \
      };
#define PTIO_OT_MAP(mapname,arrayname) std::map<int, std::string> mapname (arrayname, arrayname + sizeof (arrayname) / sizeof (std::pair<int, std::string>));


   enum PTIOS //ProvisioningTaskitemObject-Status   O_PROVISIONING_TASKITEM_OBJECT|P_STATUS
   {
      PTIOS_NOT_STARTED = 0,
      PTIOS_WORKING = 1,
      PTIOS_READY = 2,
      PTIOS_ERROR = 3,
   };
#define PTIOS_ARRAY(name) std::pair<int, std::string> name [] = { \
   std::make_pair (PTIOS_NOT_STARTED, "not started"), \
   std::make_pair (PTIOS_WORKING, "working"), \
   std::make_pair (PTIOS_READY, "ready"), \
   std::make_pair (PTIOS_ERROR, "error") \
      };
#define PTIOS_MAP(mapname,arrayname) std::map<int, std::string> mapname (arrayname, arrayname + sizeof (arrayname) / sizeof (std::pair<int, std::string>));


   enum PLE //ProvisioningLog-Executor   O_PROVISIONING_LOG|P_EXECUTOR
   {
      PLE_NONE = 0,
      PLE_DISCOVERY = 1,
      PLE_MANUAL = 2,
   };
#define PLE_ARRAY(name) std::pair<int, std::string> name [] = { \
   std::make_pair (PLE_NONE, "-"), \
   std::make_pair (PLE_DISCOVERY, "Discovery Server"), \
   std::make_pair (PLE_MANUAL, "Manual") \
      };
#define PLE_MAP(mapname,arrayname) std::map<int, std::string> mapname (arrayname, arrayname + sizeof (arrayname) / sizeof (std::pair<int, std::string>));


   enum MPLS //O_SUBTYPE|P_MPLS
   {
      MPLS_NONE = 0,
      MPLS_FORWARDING = 1,
      MPLS_EDGE = 2,
   };
#define MPLS_ARRAY(name) std::pair<int, std::string> name [] = { \
   std::make_pair (MPLS_NONE, "None"), \
   std::make_pair (MPLS_FORWARDING, "Forwarding"), \
   std::make_pair (MPLS_EDGE, "Edge") \
      };
#define MPLS_MAP(mapname,arrayname) std::map<int, std::string> mapname (arrayname, arrayname + sizeof (arrayname) / sizeof (std::pair<int, std::string>));

   // beg section BSM Adapter stuff

   // Adapter type (O_CONNECTOR_ADAPTER | P_TYPE)
   enum EAdapterType
   {
      AT_TEST_ONLY   =  0,
      AT_AM		   =  1,
      AT_ESX		   =  2,
      AT_JMX		   =  3,
      AT_E2E		   =  4,
      AT_MS_SQL	   =  5,
      AT_ORACLE	   =  6,
      AT_SAP_RFC	   =  7,
      AT_SAP_CCMS	   =  8,
      AT_SQL		   =  9,
      AT_SAP_CTRL    = 10,
      AT_XML         = 11,
      AT_SAP_LMDB	   = 12,
      AT_SAP_GMP	   = 13,
      AT_SAP_BYD	   = 14,
      AT_FLUID_OP	   = 15,
      AT_CITRIX_XEN  = 16,
      AT_SAP_BCP     = 17,
      AT_MS_EXCH2010 = 18,
      AT_LDAP		   = 19,
      AT_SAP_BLD     = 20,
      AT_CISCO_UCM    = 21,
      AT_DATA_ACCESS  = 22,
      AT_MS_HYPERV    = 23,
      AT_VANTAGE      = 24,
      AT_SAP_JMX		 = 25,
      AT_SAP          = 26,
      AT_CROSSGATE	= 27,
      AT_DB2          = 28,
      AT_LOGPARSER    = 29,
      AT_SERVICENOW   = 30,
      AT_SAP_MAXDB    = 31,
      AT_MYSQL        = 32,
      AT_SMIS         = 33,
	  AT_SCRIPT       = 34,
	  AT_QOS       	  = 36,
	  AT_HANA         = 37,
	  AT_IBMXIV_SAN   = 38,
      // insert new types here
      AT_MAX_VALUE
   };
#define ADAPTER_TYPE_ARRAY(name) std::pair<int, std::string> name [] = {\
   make_pair(AT_TEST_ONLY, "Test/Demo"),\
   make_pair(AT_AM, "Application Manager"),\
   make_pair(AT_ESX, "VMware vSphere"),\
   make_pair(AT_JMX, "JMX"),\
   make_pair(AT_E2E, "E2E"),\
   make_pair(AT_MS_SQL, "MS SQL"),\
   make_pair(AT_ORACLE, "Oracle"),\
   make_pair(AT_SAP_RFC, "SAP RFC"),\
   make_pair(AT_SAP_CCMS, "SAP CCMS"),\
   make_pair(AT_SQL, "SQL"),\
   make_pair(AT_SAP_CTRL, "SAP CTRL"),\
   make_pair(AT_XML, "XML"),\
   make_pair(AT_SAP_LMDB, "SAP LMDB"),\
   make_pair(AT_SAP_GMP, "SAP GMP"),\
   make_pair(AT_SAP_BYD, "SAP BYD"),\
   make_pair(AT_FLUID_OP, "FLUID OP"),\
   make_pair(AT_CITRIX_XEN, "Citrix XenServer"),\
   make_pair(AT_SAP_BCP, "SAP BCP"),\
   make_pair(AT_MS_EXCH2010, "MS EXCHANGE 2010"),\
   make_pair(AT_LDAP, "ADS"), \
   make_pair(AT_SAP_BLD, "SAP BLD"), \
   make_pair(AT_CISCO_UCM, "CISCO UCM"), \
   make_pair(AT_DATA_ACCESS, "DATA ACCESS"), \
   make_pair(AT_MS_HYPERV, "MS Hyper-V"), \
   make_pair(AT_VANTAGE, "VANTAGE"), \
   make_pair(AT_SAP_JMX, "SAP JMX"), \
   make_pair(AT_SAP, "SAP System"), \
   make_pair(AT_CROSSGATE, "Crossgate"), \
   make_pair(AT_DB2, "DB2"), \
   make_pair(AT_LOGPARSER, "Logparser"), \
   make_pair(AT_SERVICENOW, "ServiceNow"), \
   make_pair(AT_SAP_MAXDB, "SAP MaxDB"), \
   make_pair(AT_MYSQL, "MySQL"), \
   make_pair(AT_SMIS, "SMI-S Storage"), \
   make_pair(AT_SCRIPT, "Script"), \
   make_pair(AT_QOS, "QoS"), \
   make_pair(AT_HANA, "SAP HANA"), \
   make_pair(AT_IBMXIV_SAN, "IBM XIV SAN"), \
      };
#define ADAPTER_TYPE_MAP(mapname,arrayname) std::map<int, std::string> mapname (arrayname, arrayname + sizeof (arrayname) / sizeof (std::pair<int, std::string>));

   // enumeration value for possible predefined status of the adapter
   enum EAdapterStatus
   {
      /// remote system connected and monitored
      EAS_OK = 0,
      /// the BSM service has not instantiated the adapter
      EAS_INF_BSM_ADAPTER_STOPPED = 1,
      /// the BSM service is loading the adapter module
      EAS_INF_BSM_ADAPTER_MODULE_LOAD,
      /// the BSM service is instantiating the adapter
      EAS_INF_BSM_ADAPTER_INSTANTIATE,
      /// the BSM service is initializing the adapter
      EAS_INF_BSM_ADAPTER_INITIALIZE,
      /// the BSM service is finalizing the adapter
      EAS_INF_BSM_ADAPTER_SHUTDOWN,

      /// the connector is collecting new data
      EAS_INF_BSM_ADAPTER_DATA_COLLECT = 9,

      /// first critical error index disabling adapter reload
      EAS_ERR_BSM_1ST_CRITICAL = 10,
      /// the BSM service internals encountered an adapter independent processing error
      EAS_ERR_BSM_INTERNAL_HANDLING = EAS_ERR_BSM_1ST_CRITICAL,
      /// the BSM service can not acquire a license for this adapter
      EAS_ERR_BSM_ADAPTER_NO_LICENSE,
      /// the BSM service can not access the adapter module path
      EAS_ERR_BSM_ADAPTER_MODULE_MISSING,
      /// the BSM service is unable to instantiate the adapter or load the its module
      EAS_ERR_BSM_ADAPTER_MODULE_LOAD,
      /// the BSM service is unable to instantiate the adapter
      EAS_ERR_BSM_ADAPTER_CREATE,
      /// the BSM service adapter is of a different adapter type
      EAS_ERR_BSM_ADAPTER_TYPE,
      /// the BSM service is unable to initialize the adapter
      EAS_ERR_BSM_ADAPTER_INIT,
      /// the BSM service is unable to work with the adapter
      EAS_ERR_BSM_ADAPTER_UNAVAILABLE,
      /// the BSM service adapter (currently) does not work properly
      EAS_ERR_BSM_ADAPTER_COMM_FAIL,
      /// the BSM service adapter communication was aborted
      EAS_ERR_BSM_ADAPTER_COMM_ABORT,
      /// the cenatral BCS service has no connection to the BCS for the connector
      EAS_ERR_BSM_ADAPTER_NO_BCS_CONNECTION,

      /// last critical error index disabling adapter reload
      EAS_ERR_BSM_LST_CRITICAL = 29,

      /// first error index requiring adapter reload after a limit of subsequent failures is reached
      EAS_ERR_ADAPTER_1ST_CRITICAL = 30,
      /// adapter internals can not access BSM services or they do not work properly
      EAS_ERR_ADAPTER_SVC_MALFUNCTION = EAS_ERR_ADAPTER_1ST_CRITICAL,

      /// adapter internals do not work properly
      EAS_ERR_ADAPTER_MALFUNCTION = 50,
      /// last error index requiring adapter reload after a limit of subsequent failures is reached
      EAS_ERR_ADAPTER_LST_CRITICAL = 59,

      /// first error index requiring adapter reload after a limit of subsequent failures is reached
      EAS_ERR_ADAPTER_1ST_FATAL = 60,
      /// adapter internals aborted working
      EAS_ERR_ADAPTER_FATAL_MALFUNCTION = EAS_ERR_ADAPTER_1ST_FATAL,

      /// last error index requiring adapter reload after a limit of subsequent failures is reached
      EAS_ERR_ADAPTER_LST_FATAL = 69,

      /// first error index NOT requiring adapter reload
      EAS_ERR_ADAPTER_1ST_HARMLESS = 70,
      /// remote system cannot be connected
      EAS_ERR_TARGET_SYSTEM_UNREACHABLE = EAS_ERR_ADAPTER_1ST_HARMLESS,
      /// remote system exists but cannot be queried
      /// (e.g. due to improper login data)
      EAS_ERR_TARGET_SYSTEM_UNAVAILABLE,
      /// remote system exists but does not work properly
      EAS_ERR_TARGET_SYSTEM_MALFUNCTION,
      /// no access to remote system 
      EAS_ERR_TARGET_SYSTEM_NO_ACCESS,

      /// last error index NOT requiring adapter reload
      EAS_ERR_ADAPTER_LST_HARMLESS = 99,
   };

#define ADAPTER_STATUS_ARRAY(name) std::pair<int, std::string> name [] = {\
   std::make_pair(EAS_OK, "OK"),\
   std::make_pair(EAS_INF_BSM_ADAPTER_STOPPED, "STOPPED"),\
   std::make_pair(EAS_INF_BSM_ADAPTER_MODULE_LOAD, "MODULELOAD"),\
   std::make_pair(EAS_INF_BSM_ADAPTER_INSTANTIATE, "CREATE"),\
   std::make_pair(EAS_INF_BSM_ADAPTER_INITIALIZE, "INITIALIZE"),\
   std::make_pair(EAS_INF_BSM_ADAPTER_SHUTDOWN, "SHUTDOWN"),\
   std::make_pair(EAS_INF_BSM_ADAPTER_DATA_COLLECT, "COLLECT_DATA"),\
   std::make_pair(EAS_ERR_BSM_INTERNAL_HANDLING, "service internal error"),\
   std::make_pair(EAS_ERR_BSM_ADAPTER_NO_LICENSE, "no license"),\
   std::make_pair(EAS_ERR_BSM_ADAPTER_MODULE_MISSING, "module missing"),\
   std::make_pair(EAS_ERR_BSM_ADAPTER_MODULE_LOAD, "module load failed"),\
   std::make_pair(EAS_ERR_BSM_ADAPTER_CREATE, "create failed"),\
   std::make_pair(EAS_ERR_BSM_ADAPTER_TYPE, "wrong type"),\
   std::make_pair(EAS_ERR_BSM_ADAPTER_INIT, "init failed"),\
   std::make_pair(EAS_ERR_BSM_ADAPTER_UNAVAILABLE, "unavailable"),\
   std::make_pair(EAS_ERR_BSM_ADAPTER_COMM_FAIL, "BCS communication failed"),\
   std::make_pair(EAS_ERR_BSM_ADAPTER_COMM_ABORT, "BCS communication aborted"),\
   std::make_pair(EAS_ERR_BSM_ADAPTER_NO_BCS_CONNECTION, "No connection to BCS service"),\
   std::make_pair(EAS_ERR_ADAPTER_SVC_MALFUNCTION, "BCS services unavailable"),\
   std::make_pair(EAS_ERR_ADAPTER_MALFUNCTION, "adapter malfunction"),\
   std::make_pair(EAS_ERR_ADAPTER_FATAL_MALFUNCTION, "adapter fatal malfunction"),\
   std::make_pair(EAS_ERR_TARGET_SYSTEM_UNREACHABLE, "target system unreachable"),\
   std::make_pair(EAS_ERR_TARGET_SYSTEM_UNAVAILABLE, "target system unavailable"),\
   std::make_pair(EAS_ERR_TARGET_SYSTEM_MALFUNCTION, "target system malfunction"),\
   std::make_pair(EAS_ERR_TARGET_SYSTEM_NO_ACCESS, "target system inaccessible")\
      };

#define ADAPTER_STATUS_MAP(mapname,arrayname) std::map<int, std::string> mapname (arrayname, arrayname + sizeof (arrayname) / sizeof (std::pair<int, std::string>));


   /// for additional hints in respect to BC installation requirements >>>
   /// to be used on connector status EAS_ERR_BSM_ADAPTER_MODULE_LOAD and EAS_ERR_BSM_ADAPTER_CREATE
#define MODULE_LOAD_FAIL_SZ_NONE		"The connector assembly could not be loaded. No specific installation requirement hints are provided. Please contact support."

#define MODULE_LOAD_FAIL_SZ_AT_JMX		"Please verify that you have properly installed Java Runtime and added the server or client directory to the system PATH environment variable. Moreover check if you need to use the 64-bit or 32-bit Java installation and rename the corresponding JVMBridge_x??.exe to JVMBridge.exe in the BCS service directory."
#define MODULE_LOAD_FAIL_SZ_AT_MS_SQL	"Please verify that you have properly installed SQL Server (2008) Client Connectivity Tools [SQLClient]."
#define MODULE_LOAD_FAIL_SZ_AT_ORACLE	"Please verify that you have properly installed Oracle Data Access Components with Oracle Data Provider for .NET 4.0."

#define ADAPTER_TYPE_MODULE_LOAD_FAIL_ARRAY(name) std::pair<int, std::string> name [] = {\
   make_pair(AT_TEST_ONLY, MODULE_LOAD_FAIL_SZ_NONE),\
   make_pair(AT_AM, 		MODULE_LOAD_FAIL_SZ_NONE),\
   make_pair(AT_ESX, 		MODULE_LOAD_FAIL_SZ_NONE),\
   make_pair(AT_JMX, 		MODULE_LOAD_FAIL_SZ_AT_JMX),\
   make_pair(AT_E2E, 		MODULE_LOAD_FAIL_SZ_NONE),\
   make_pair(AT_MS_SQL, 	MODULE_LOAD_FAIL_SZ_AT_MS_SQL),\
   make_pair(AT_ORACLE, 	MODULE_LOAD_FAIL_SZ_AT_ORACLE),\
   make_pair(AT_SAP_RFC, 	MODULE_LOAD_FAIL_SZ_NONE),\
   make_pair(AT_SAP_CCMS, 	MODULE_LOAD_FAIL_SZ_NONE),\
   make_pair(AT_SQL, 		MODULE_LOAD_FAIL_SZ_NONE),\
   make_pair(AT_SAP_CTRL, 	MODULE_LOAD_FAIL_SZ_NONE),\
   make_pair(AT_XML, 		MODULE_LOAD_FAIL_SZ_NONE),\
   make_pair(AT_SAP_LMDB, 	MODULE_LOAD_FAIL_SZ_NONE),\
   make_pair(AT_SAP_GMP, 	MODULE_LOAD_FAIL_SZ_NONE),\
   make_pair(AT_SAP_BYD, 	MODULE_LOAD_FAIL_SZ_NONE),\
   make_pair(AT_FLUID_OP, 	MODULE_LOAD_FAIL_SZ_NONE),\
   make_pair(AT_CITRIX_XEN, MODULE_LOAD_FAIL_SZ_NONE),\
   make_pair(AT_SAP_BCP, 	MODULE_LOAD_FAIL_SZ_NONE),\
   make_pair(AT_MS_EXCH2010, MODULE_LOAD_FAIL_SZ_NONE),\
   make_pair(AT_LDAP, 		MODULE_LOAD_FAIL_SZ_NONE), \
   make_pair(AT_SAP_BLD, 	MODULE_LOAD_FAIL_SZ_NONE), \
   make_pair(AT_CISCO_UCM, MODULE_LOAD_FAIL_SZ_NONE), \
   make_pair(AT_DATA_ACCESS, MODULE_LOAD_FAIL_SZ_NONE), \
   make_pair(AT_MS_HYPERV, MODULE_LOAD_FAIL_SZ_NONE), \
   make_pair(AT_VANTAGE, 	MODULE_LOAD_FAIL_SZ_NONE), \
   make_pair(AT_SAP_JMX, 	MODULE_LOAD_FAIL_SZ_AT_JMX), \
   make_pair(AT_SAP, 		MODULE_LOAD_FAIL_SZ_NONE), \
   make_pair(AT_CROSSGATE, MODULE_LOAD_FAIL_SZ_NONE), \
   make_pair(AT_DB2,       MODULE_LOAD_FAIL_SZ_NONE), \
   make_pair(AT_LOGPARSER, MODULE_LOAD_FAIL_SZ_NONE), \
   make_pair(AT_SERVICENOW,MODULE_LOAD_FAIL_SZ_NONE), \
   make_pair(AT_SAP_MAXDB, MODULE_LOAD_FAIL_SZ_NONE), \
   make_pair(AT_MYSQL,     MODULE_LOAD_FAIL_SZ_NONE), \
   make_pair(AT_SMIS,      MODULE_LOAD_FAIL_SZ_NONE), \
   make_pair(AT_SCRIPT,    MODULE_LOAD_FAIL_SZ_NONE), \
   make_pair(AT_QOS,       MODULE_LOAD_FAIL_SZ_NONE), \
   make_pair(AT_HANA,      MODULE_LOAD_FAIL_SZ_NONE), \
   make_pair(AT_IBMXIV_SAN,      MODULE_LOAD_FAIL_SZ_NONE), \
      };
#define ADAPTER_TYPE_MODULE_LOAD_FAIL_MAP(mapname,arrayname) std::map<int, std::string> mapname (arrayname, arrayname + sizeof (arrayname) / sizeof (std::pair<int, std::string>));
   /// for additional hints in respect to BC installation requirements <<<

   /// for additional hints in respect to BC initialization faiulres >>>
   /// to be used on connector status EAS_ERR_BSM_ADAPTER_INIT
#define MODULE_INIT_FAIL_SZ_AT_JMX		"Please verify that you have properly installed Java Runtime and added the server or client directory to the system PATH environment variable. Moreover check if you need to use the 64-bit or 32-bit Java installation and rename the corresponding JVMBridge_x??.exe to JVMBridge.exe in the BCS service directory."

#define ADAPTER_TYPE_MODULE_INIT_FAIL_ARRAY(name) std::pair<int, std::string> name [] = {\
   make_pair(AT_JMX, 		MODULE_INIT_FAIL_SZ_AT_JMX),\
   make_pair(AT_SAP_JMX, 	MODULE_INIT_FAIL_SZ_AT_JMX),\
      };
#define ADAPTER_TYPE_MODULE_INIT_FAIL_MAP(mapname,arrayname) std::map<int, std::string> mapname (arrayname, arrayname + sizeof (arrayname) / sizeof (std::pair<int, std::string>));
   /// for additional hints in respect to BC initialization faiulres <<<

   /// enumeration for bsm server status
   enum BCServerStatusColor
   {
		BCS_STATUS_GREEN  = 0,
		BCS_STATUS_GRAY   = 0x40000000,
		BCS_STATUS_YELLOW = 0x80000000,
		BCS_STATUS_RED 	  = 0xC0000000,
   };

   enum BCServerStatus
   {
	  BCS_STATUS_OK 		= 0 | BCS_STATUS_GREEN,
	  BCS_STATUS_UNKNOWN 	= 1 | BCS_STATUS_GRAY,
	  BCS_STATUS_INACTIVE 	= 2 | BCS_STATUS_GRAY,
	  BCS_STATUS_LOADING 	= 3 | BCS_STATUS_YELLOW,
	  BCS_STATUS_WARNING 	= 4 | BCS_STATUS_YELLOW,
	  BCS_STATUS_ERROR 		= 5 | BCS_STATUS_RED,
	  BCS_STATUS_EXCEPTION 	= 6 | BCS_STATUS_RED,
      BCS_STATUS_STOPPED 	= 6 | BCS_STATUS_GRAY,
   };


#define BCS_STATUS_ARRAY(name) std::pair<int, std::string> name [] = {\
   std::make_pair(BCS_STATUS_OK, "OK"),\
   std::make_pair(BCS_STATUS_INACTIVE, "Inactive"),\
   std::make_pair(BCS_STATUS_LOADING, "Loading"),\
   std::make_pair(BCS_STATUS_WARNING, "Warning"),\
   std::make_pair(BCS_STATUS_ERROR, "Error"),\
   std::make_pair(BCS_STATUS_EXCEPTION, "Exception"),\
   std::make_pair(BCS_STATUS_STOPPED, "Stopped"),\
      };

#define BCS_STATUS_MAP(mapname,arrayname) std::map<int, std::string> mapname (arrayname, arrayname + sizeof (arrayname) / sizeof (std::pair<int, std::string>));

   /// enumeration for bsm adapter object status
   enum EAdapterObjectStatus
   {
      /// object status undefined, i.e. the object may have been removed
      EOS_UNDEF = 0,
      /// object is up, i.e. object is accessible and provides data
      EOS_OK,
      /// object is known to be down or unavailable
      EOS_DOWN,
      /// object not monitored, i.e. object is existent but inaccessible or does not provide data
      EOS_NOTMON
   };

   // required to map strings into nmdbid.NET
#define ADAPTEROBJECTSTATUS_SZ_UNDEF 	"inaccessible"
#define ADAPTEROBJECTSTATUS_SZ_OK		"ok"
#define ADAPTEROBJECTSTATUS_SZ_DOWN		"not runnning"
#define ADAPTEROBJECTSTATUS_SZ_NOTMON 	"not monitored"

#define ADAPTER_OBJECT_STATUS_ARRAY(name) std::pair<int, std::string> name [] = {\
   std::make_pair(EOS_UNDEF, ADAPTEROBJECTSTATUS_SZ_UNDEF),\
   std::make_pair(EOS_OK, ADAPTEROBJECTSTATUS_SZ_OK),\
   std::make_pair(EOS_DOWN, ADAPTEROBJECTSTATUS_SZ_DOWN),\
   std::make_pair(EOS_NOTMON, ADAPTEROBJECTSTATUS_SZ_NOTMON),\
      };

#define ADAPTER_OBJECT_STATUS_MAP(mapname,arrayname) std::map<int, std::string> mapname (arrayname, arrayname + sizeof (arrayname) / sizeof (std::pair<int, std::string>));

   /// bsm adapter basic error (return) types
   enum EAdapterErrorBase
   {
      /// success
      EAEB_OK = 0,
      /// failure
      EAEB_FAIL = 1,
      /// missing or insufficient input data
      EAEB_MISSING_INPUT,
      /// invalid input data
      EAEB_INVALID_INPUT,
      /// unable to query data as system is not connected
      EAEB_NOT_CONNECTED,
      /// remote system delivered an error
      EAEB_REMOTE_ERROR,
      /// method invocation or parameter not supported
      EAEB_NOT_SUPPORTED,
      /// permission is missing for a requested method invocation
      EAEB_NO_PERMISSION,
      /// skipped operation of (part of) the request
      EAEB_SKIPPED_OP,
      /// internal processing error occurred or unexpected exception encountered
      EAEB_PROCESSING_ERROR,
      /// BSM Server encountered an unhandled exception; see BSM logs for details
      EAEB_UNHANDLED_EXCEPTION,
      /// the requested adapter is disabled
      EAEB_ADAPTER_DISABLED,
      /// the requested adapter is unaccessible
      EAEB_ADAPTER_LOAD_FAILURE,
      /// the requested adapter is in load process
      EAEB_ADAPTER_LOADING,
      /// the requested adapter is not known to the system
      EAEB_ADAPTER_UNKNOWN,
      /// the requested adapter is currently initializing
      EAEB_ADAPTER_INITIALIZING,
      /// the requested reference was not found
      EAEB_REF_NOT_FOUND,
      /// no license available for processing
      EAEB_ADAPTER_NOT_LICENSED,
      /// new with 7.1 SP6: service is busy
      EAEB_SERVICE_BUSY,
      /// new with 7.1 SP6: call is cancelled externally
      EAEB_CALL_CANCELLED,
   };

   // required to map strings into nmdbid.NET
#define EADAPTERERRORBASE_SZ_SUCCESS				"success"
#define EADAPTERERRORBASE_SZ_FAILURE				"failure"
#define EADAPTERERRORBASE_SZ_MISSING_INPUT			"missing or insufficient input data"
#define EADAPTERERRORBASE_SZ_INVALID_INPUT			"invalid input data"
#define EADAPTERERRORBASE_SZ_NOT_CONNECTED			"unable to query data as system is not connected"
#define EADAPTERERRORBASE_SZ_REMOTE_ERROR			"remote system delivered an error"
#define EADAPTERERRORBASE_SZ_NOT_SUPPORTED			"method invocation or parameter not supported"
#define EADAPTERERRORBASE_SZ_NO_PERMISSION			"permission is missing for a requested method invocation"
#define EADAPTERERRORBASE_SZ_SKIPPED_OP				"skipped operation of (part of) the request"
#define EADAPTERERRORBASE_SZ_PROCESSING_ERROR		"internal processing error occurred or unexpected exception encountered"
#define EADAPTERERRORBASE_SZ_UNHANDLED_EXCEPTION	"BCS Service encountered an unhandled exception; see BCS logs for details"
#define EADAPTERERRORBASE_SZ_ADAPTER_DISABLED		"The requested connector is disabled."
#define EADAPTERERRORBASE_SZ_ADAPTER_LOAD_FAILURE	"The requested connector is unaccessible."
#define EADAPTERERRORBASE_SZ_ADAPTER_LOADING		"The requested connector is in load process."
#define EADAPTERERRORBASE_SZ_ADAPTER_UNKNOWN		"The requested connector is not known to the system."
#define EADAPTERERRORBASE_SZ_ADAPTER_INITIALIZING	"The requested connector is currently initializing."
#define EADAPTERERRORBASE_SZ_REF_NOT_FOUND			"The requested reference was not found."
#define EADAPTERERRORBASE_SZ_ADAPTER_NOT_LICENSED	"no license available for processing"

#define ADAPTER_ERROR_BASE_ARRAY(name) std::pair<int, std::string> name [] = {\
   std::make_pair(EAEB_OK, EADAPTERERRORBASE_SZ_SUCCESS),\
   std::make_pair(EAEB_FAIL, EADAPTERERRORBASE_SZ_FAILURE),\
   std::make_pair(EAEB_MISSING_INPUT, EADAPTERERRORBASE_SZ_MISSING_INPUT),\
   std::make_pair(EAEB_INVALID_INPUT, EADAPTERERRORBASE_SZ_INVALID_INPUT),\
   std::make_pair(EAEB_NOT_CONNECTED, EADAPTERERRORBASE_SZ_NOT_CONNECTED),\
   std::make_pair(EAEB_REMOTE_ERROR, EADAPTERERRORBASE_SZ_REMOTE_ERROR),\
   std::make_pair(EAEB_NOT_SUPPORTED, EADAPTERERRORBASE_SZ_NOT_SUPPORTED),\
   std::make_pair(EAEB_NO_PERMISSION, EADAPTERERRORBASE_SZ_NO_PERMISSION),\
   std::make_pair(EAEB_SKIPPED_OP, EADAPTERERRORBASE_SZ_SKIPPED_OP),\
   std::make_pair(EAEB_PROCESSING_ERROR, EADAPTERERRORBASE_SZ_PROCESSING_ERROR),\
   std::make_pair(EAEB_UNHANDLED_EXCEPTION, EADAPTERERRORBASE_SZ_UNHANDLED_EXCEPTION),\
   std::make_pair(EAEB_ADAPTER_DISABLED, EADAPTERERRORBASE_SZ_ADAPTER_DISABLED),\
   std::make_pair(EAEB_ADAPTER_LOAD_FAILURE, EADAPTERERRORBASE_SZ_ADAPTER_LOAD_FAILURE),\
   std::make_pair(EAEB_ADAPTER_LOADING, EADAPTERERRORBASE_SZ_ADAPTER_LOADING),\
   std::make_pair(EAEB_ADAPTER_UNKNOWN, EADAPTERERRORBASE_SZ_ADAPTER_UNKNOWN),\
   std::make_pair(EAEB_ADAPTER_INITIALIZING, EADAPTERERRORBASE_SZ_ADAPTER_INITIALIZING),\
   std::make_pair(EAEB_REF_NOT_FOUND, EADAPTERERRORBASE_SZ_REF_NOT_FOUND),\
   std::make_pair(EAEB_ADAPTER_NOT_LICENSED, EADAPTERERRORBASE_SZ_ADAPTER_NOT_LICENSED),\
      };

#define ADAPTER_ERROR_BASE_MAP(mapname,arrayname) std::map<int, std::string> mapname (arrayname, arrayname + sizeof (arrayname) / sizeof (std::pair<int, std::string>));


   // AM Authenctication Type (O_AUTHENTICATION | P_SECURITYTYPE)
   enum EAMAuthenticationType //
   {
      AAT_SSO = 0,
      AAT_IDM = 1,
      AAT_WINDOWS = 2
   };

   // end section BSM/BCS Adapter stuff

   // E-Mail Account Type (O_MAIL_ACCOUNTS | P_TYPE)
   enum EMailAccountType //
   {
      MAIL_POP3 = 0,
      MAIL_IMAP = 1,
   };
#define EMAIL_TYPE_ARRAY(name) std::pair<int, std::string> name [] = { \
   std::make_pair (MAIL_POP3, "POP3"), \
   std::make_pair (MAIL_IMAP, "IMAP") \
      };
#define EMAIL_TYPE_MAP(mapname,arrayname) std::map<int, std::string> mapname (arrayname, arrayname + sizeof (arrayname) / sizeof (std::pair<int, std::string>));

   // E-Mail Account Source (O_MAIL_ACCOUNTS | P_SOURCE)
   enum EMailAccountSource //
   {
      MAIL_SRC_TRAPSERVER = 0,
   };

   enum EMailAccountStatus
   {
      MAIL_STATUS_OK = 0,
      MAIL_STATUS_INACTIVE,
      MAIL_STATUS_LOADING,
      MAIL_STATUS_WARNING,
      MAIL_STATUS_ERROR,
      MAIL_STATUS_EXCEPTION,
   };


#define MAIL_ACCOUNT_STATUS_ARRAY(name) std::pair<int, std::string> name [] = {\
   std::make_pair(MAIL_STATUS_OK, "OK"),\
   std::make_pair(MAIL_STATUS_INACTIVE, "Inactive"),\
   std::make_pair(MAIL_STATUS_LOADING, "Loading"),\
   std::make_pair(MAIL_STATUS_WARNING, "Warning"),\
   std::make_pair(MAIL_STATUS_ERROR, "Error"),\
   std::make_pair(MAIL_STATUS_EXCEPTION, "Exception"),\
      };

#define MAIL_ACCOUNT_STATUS_MAP(mapname,arrayname) std::map<int, std::string> mapname (arrayname, arrayname + sizeof (arrayname) / sizeof (std::pair<int, std::string>));

   // BSM/BCS Event Filter ID ( O_BSM_EVENT_FILTER | P_ID )
   enum BSMEventFilter
   {
      BSM_EV_FLT_DEFAULT = -1,
   };

   // ServerMonitor (Xml-)Type ( O_SERVER_MONITOR | P_TYPE )
   enum SMT
   {
      SMT_UNKNOWN = 0,
      SMT_EVENT = 1,
      SMT_PROCESS = 2,
      SMT_SERVICE = 3,
      SMT_LOGFILE = 4,
      SMT_PERFORMANCE = 5,
      SMT_ACTION = 6,
      SMT_TASK = 7,
   };

   // ServerMonitor threshold mapping type (O_SERVER_MONITOR_THRESHOLD_MAPPING | P_TYPE)
   enum SMTMT
   {
      SMTMT_PROCESSOR = 1,
      SMTMT_MEMORY = 2,
      SMTMT_DISK = 3,
		SMTMT_LOGPARSER_STATUS = 4,
      SMTMT_PROCESS = 5,
   };

   // ServerMonitor log event type (O_SERVER_MONITOR_EVENT_LOG_POLL | P_EVENT_TYPE)
   enum SMLET
   {
      SMLET_ALL = 0,
      SMLET_ERROR = 1,
      SMLET_WARNING,
      SMLET_INFORMATION,
      SMLET_SECURITY_AUDIT_SUCCESS,
      SMLET_SECURITY_AUDIT_FAILURE,
      SMLET_CRITICAL,
      SMLET_VERBOSE,
   };

   inline const char* SMLogEventTypeToString (int type, const char* pDefault = "Unknown")
   {
      switch (type)
      {
      case SMLET_ALL: return "All";
      case SMLET_ERROR: return "Error";
      case SMLET_WARNING: return "Warning";
      case SMLET_INFORMATION: return "Information";
      case SMLET_SECURITY_AUDIT_SUCCESS: return "Security Audit Success";
      case SMLET_SECURITY_AUDIT_FAILURE: return "Security Audit Failure";
      case SMLET_CRITICAL: return "Critical";
      case SMLET_VERBOSE: return "Verbose";
      }
      return pDefault;
   }

   // ServerMonitor service status (O_SERVER_MONITOR_SERVICE_STATUS_MAPPING | P_SERVICE_STATUS)
   enum SMSS
   {
      SMSS_STOPPED = 0,
      SMSS_STARTED = 1,
      SMSS_PAUSED = 2,
      SMSS_STOPPING = 3,
      SMSS_STARTING = 4
   };

   inline const char* SMServiceStatusToString (int status, const char* pDefault = "Unknown")
   {
      switch (status)
      {
      case SMSS_STOPPED: return "Stopped";
      case SMSS_STARTED: return "Started";
      case SMSS_PAUSED: return "Paused";
      case SMSS_STOPPING: return "Stopping";
      case SMSS_STARTING: return "Starting";
      }
      return pDefault;
   }

   // ServerMonitor process status (O_SERVER_MONITOR_PROCESS_STATUS_MAPPING | P_PROCESS_STATUS)
   enum SMPS
   {
      SMPS_STOPPED = 0,
      SMPS_STARTED = 1,
   };

   inline const char* SMProcessStatusToString (int status, const char* pDefault = "Unknown")
   {
      switch (status)
      {
      case SMPS_STOPPED: return "Stopped";
      case SMPS_STARTED: return "Started";
      }
      return pDefault;
   }

   // ServerMonitor query type (O_SERVER_MONITOR_xxx_POLL | P_QUERY_TYPE)
   enum SMQT
   {
      SMQT_NONE = 0,
      SMQT_WMI,
      SMQT_POWERSHELL,
   };

   inline const char* SMQueryTypeToString (int type, const char* pDefault = "Unknown")
   {
      switch (type)
      {
      case SMQT_NONE: return "None";
      case SMQT_WMI: return "WMI";
      case SMQT_POWERSHELL: return "PowerShell";
      }
      return pDefault;
   }

   // ServerMonitor data type (O_SERVER_MONITOR_POLL_TEMPLATE | P_TYPE)
   enum SMDT
   {
      SMDT_AUTHENTICATION = 1,
      SMDT_NODE = 2,
      SMDT_SERVICE = 3,
      SMDT_PROCESS = 4,
      SMDT_EVENTLOG = 5,
      SMDT_LOGFILE = 6,
      SMDT_FILE = 7,
      SMDT_FOLDER = 8,
      SMDT_MEMORY = 9,
      SMDT_DISK = 10,
      SMDT_PROCESSOR = 11,
      SMDT_PACKAGE = 12,
      SMDT_LINK = 13,
      SMDT_LOGPARSER_STATUS = 14,
      SMDT_LOGPARSER_EVENT = 15,
   };
   inline const char* SMDataTypeToString (int type, const char* pDefault = "Unknown")
   {
      switch (type)
      {
      case SMDT_AUTHENTICATION: return "Authentication";
      case SMDT_NODE: return "Node";
      case SMDT_SERVICE: return "Service";
      case SMDT_PROCESS: return "Process";
      case SMDT_EVENTLOG: return "Event Log";
      case SMDT_LOGFILE: return "Log File";
      case SMDT_FILE: return "File";
      case SMDT_FOLDER: return "Folder";
      case SMDT_MEMORY: return "Memory";
      case SMDT_DISK: return "Volume";
      case SMDT_PROCESSOR: return "Processor Aggregate";
      case SMDT_PACKAGE: return "Package";
      case SMDT_LINK: return "Link";
      case SMDT_LOGPARSER_STATUS: return "Logparser Status";
      case SMDT_LOGPARSER_EVENT: return "Logparser Event";
      }
      return pDefault;
   }

   // ServerMonitor site type
   enum SMST
   {
      SMST_DEFAULT = -1,
      SMST_LOCAL = 0,
      SMST_CENTRAL = 1,
   };
   inline const char* SMSiteTypeToString (int type, const char* pDefault = "Unknown")
   {
      switch (type)
      {
      case SMST_DEFAULT: return "Default";
      case SMST_LOCAL: return "Local";
      case SMST_CENTRAL: return "Central";
      }
      return pDefault;
   }

   // ServerMonitor service start mode (O_SERVER_MONITOR_SERVICE_POLL | P_START_MODE)
   enum SMSSM
   {
      SMSSM_AUTOMATIC = 2,
      SMSSM_MANUAL = 3,
      SMSSM_DISABLED = 4,
      SMSSM_ALL = 10,
   };
   inline const char* SMServiceStartModeToString (int mode, const char* pDefault = "Unknown")
   {
      switch (mode)
      {
      case SMSSM_AUTOMATIC: return "Automatic";
      case SMSSM_MANUAL: return "Manual";
      case SMSSM_DISABLED: return "Disabled";
      case SMSSM_ALL: return "All";
      }
      return pDefault;
   }

   // ServerMonitor Filter type (O_SERVER_MONITOR_FILTER_EVENT | P_FILTER_TYPE)
   enum SMFilterType
   {
      SMFilterType_BLACKLIST
   };
   inline const char* SMFiltertypeToString(int filterType, const char* pDefault = "Unknown")
   {
      switch(filterType)
      {
      case SMFilterType_BLACKLIST: return "Blacklist";
      }
      return pDefault;
   }

   // O_POLL | P_LICENSE_STATUS
   enum PollLicenseStatus
   {
      LS_NONE = 0,
      LS_PENDING = 1,
      LS_GRANTED = 2,
      LS_DENIED = 3,
   };

   enum _NODE_OBJECT_TYPE
   {
      // Node object type
      O_NODE			= 0x2000,

      O_NODEGROUP     = 0x2020,
      O_GROUP   = O_NODEGROUP,

      O_NODEGROUPNODE = 0x2040,
      O_POLL_GROUP_ASSIGNMENT = O_NODEGROUPNODE,
      O_MO_GROUP_ASSIGNMENT = O_NODEGROUPNODE,
      O_NODE_TEMPLATE_GROUP_ASSIGNMENT = O_NODEGROUPNODE,
      O_MO_TEMPLATE_GROUP_ASSIGNMENT = O_NODEGROUPNODE,
      O_PROV_GROUP_ASSIGNMENT = O_NODEGROUPNODE,
      //last
      O_GROUP_ASSIGNMENT = O_NODEGROUPNODE,


      O_ADDRESS       = 0x2060,

      O_POLL_TEMPLATE = 0x2080,
      O_POLL          = O_POLL_TEMPLATE,

      O_HISTORYPOLL	= 0x2082,
      O_SNMPPOLL		= 0x20A0,
      O_HTTPPOLL		= 0x20C0,
      O_TCPPOLL		= 0x20E0,
      O_WINNETPOLL    = 0x2100,
      O_SUBTYPE		= 0x2120,
      O_SUBTYPE_PROP			= 0x212A,
      O_MOD_SUBTYPE_PROP	= 0x212B,
      O_DUAL_SUBTYPE_PROP			= 0x212C,
      O_MOD_DUAL_SUBTYPE_PROP	= 0x212D,

      O_SUBTYPEPARAM  = 0x2130,
      O_MAINTENANCE   = 0x2140,
      O_THRESHOLD		= 0x2150,
      O_THRESHOLDITEM = 0x2151,
      O_THRESHOLDREF  = 0x2152,
      O_HISTORY		= 0x215F,
      O_SNMP			= 0x2160,
      O_BROWSE		= 0x2161,
      O_SNMPOVERHTTP  = 0x2162,
      O_ADDRESSSUBTYPE = 0x2163,
      O_HTTP			= 0x2164,
      O_TCP			= 0x2165,
      O_WINNET		= 0x2166,
      O_PING			= 0x2167,
      O_SYSINFO		= 0x2168,
      O_CUSTOMINFO	= 0x2169,
      O_SUBTYPERELATION = 0x216A,
      O_DEVICEMANAGER = 0x216B,
      O_ERMGROUPNODE	= 0x216C,
      O_AUTHENTICATION = 0x216D,
      O_SNMPENGINEID = 0x216E,
      O_MIBACCESS = 0x216F,


      O_CORRELATION_TEMPLATE = 0x2170,
      O_CORRELATION          = O_CORRELATION_TEMPLATE,

      O_CORRTEMPLATE_RULE = 0x2171,
      O_CORR_RULE         = O_CORRTEMPLATE_RULE,

      O_CORRTEMPLATE_FUNCTION = 0x2172,
      O_CORR_FUNCTION         = O_CORRTEMPLATE_FUNCTION,

      O_CORRTEMPLATE_VALUE = 0x2173,
      O_CORR_VALUE         = O_CORRTEMPLATE_VALUE,

      O_CORRTEMPLATE_OBJECT = 0x2174,
      O_CORR_OBJECT         = O_CORRTEMPLATE_OBJECT,


      O_STATUS_POLL     = 0x2175,
      O_STATUS_MO       = 0x2176, //NMSubtype
      O_STATUS_NODE     = 0x2177, //Node or Grouping Element
      O_STATUS_MOPROP	= 0x2178, //Subtype-Property-Value
      O_STATUS_LINE     = 0x2179, //NMSubtypeRelation

      O_ADDRESS_NET				= 0x217A,
      O_ADDRESS_ADDRESS			= 0x217B,
      O_ADDRESS_RESERVATION	= 0x217C,

      O_GE_ELEMENTS		= 0x217D,

      O_PROVISIONING_CONFIG = 0x217E,
      O_NODEIMAGE = 0x217F,
      O_SUBTYPE_VARIABLE = 0x2180,
      O_AUTOIMAGE = 0x2181,

      O_ADDRESS_NATTING	= 0x2182,
      O_XML_FILE	= 0x2183,

      O_NODE_TEMPLATE = 0x2184,
      O_MO_TEMPLATE = 0x2185,
      O_TEMPLATEREF = 0x2186,
      O_POLL_TEMPLATE_DISTRIBUTION = 0x2187,
      O_NODE_TEMPLATE_SYSTEM = 0x2188,
      O_MO_TEMPLATE_SYSTEM = 0x2189,

      O_PROVISIONING_LOG = 0x218F,

      O_PROVISIONING_NODE = 0x2190,
      O_PROVISIONING_SCRIPT = 0x2192,

      O_PROVISIONING_TASK = 0x2194,
      O_PROVISIONING_TASKITEM = 0x2196,
      O_PROVISIONING_TASKITEM_OBJECT = 0x2198,
      O_ROUTING = 0x2199,
      O_MOD_GROUP = 0x219a,
      O_MOD_CMDB =  0x219b,
      O_MOD_NODEGROUP =  0x219c,
      O_MOD_NODE =  0x219d,
      O_MOD_SUBTYPE =  0x219e,
      O_NODE_PATH = 0x219f,
      O_STATEHISTORY = 0x21A0,
      O_GROUPADDRESS = 0x21A1,
      O_ADDRESSGROUP = 0x21A2,
      O_SINGLE_PATH = 0x21A3,
      O_SINGLE_PATH_ELEMENT = 0x21A4,
      O_SUBAGENT = 0x21A5,
      O_CONNECTOR_ADAPTER = 0x21A6,
      O_SUBAGENTTYPE = 0x21A7,
      O_ADAPTER_PLUGIN = 0x21A8,
      O_ADAPTER_PARAMETER = 0x21A9,
      O_MOD_GE_ELEMENTS = 0x21AA,
      O_DASHBOARD_PARAMETER = 0x21AB,
      O_GE_ELEMENT_ADAPTER = 0x21AC,
      O_MAIL_ACCOUNTS = 0x21AD,
      O_BSM_EVENT_FILTER = 0x21AE,
      O_BSM_EVENT_FILTER_RULE = 0x21AF,
      O_WMI = 0x21B2,
      O_BSM = 0x21B3,
      O_BSM_EVENT_CATEGORY = 0x21B4,
      O_BSM_EVENT_CLASS = 0x21B5,
      O_BSM_ELEMENT = 0x21B6,
      O_REQUEST_SEQUENCE = 0x21B7,
      O_REQUEST_ELEMENT = 0x21B8,
      O_REQUEST_ELEMENT_INT = 0x21B9,
      O_REQUEST_ELEMENT_BOOL = 0x21BA,
      O_REQUEST_ELEMENT_STRING = 0x21BB,
      O_OBJECT_FILTER = 0x21BC,
      O_ADAPTER_LICENSING = 0x21BD,
      O_SERVER_MONITOR = 0x21BE,
      O_SERVER_MONITOR_XML = 0x21BF,
      O_CI_RELATION = 0x21C1,
      O_SERVER_MONITOR_POLL_TEMPLATE = 0x21C2,
      O_SERVER_MONITOR_SERVICE_POLL = 0x21C5,
      O_SERVER_MONITOR_PROCESS_POLL = 0x21C6,
      O_SERVER_MONITOR_EVENT_LOG_POLL = 0x21C7,
      O_SERVER_MONITOR_EVENT_LOG_STATUS_MAPPING = 0x21C8,
      O_SERVER_MONITOR_PROCESSOR_POLL = 0x21C9,
      O_SERVER_MONITOR_DISK_POLL = 0x21CA,
      O_SERVER_MONITOR_MEMORY_POLL = 0x21CB,
      O_SERVER_MONITOR_THRESHOLD_MAPPING = 0x21CC,
      O_SERVER_MONITOR_SERVICE_STATUS_MAPPING = 0x21CE,
      //= 0x21CF,
      O_LOGPARSER_STATUS_POLL = 0x21D0,
      O_LOGPARSER_EVENT_POLL = 0x21D1,
      O_LOGPARSER_REGEX = 0x21D2,
      O_LOGPARSER = 0x21D3,
      O_CONNECTOR_PERFORMANCE_COUNTER = 0x21D4,
      O_LOGPARSER_LAST_STATE = 0x21D5,
      O_SERVER_MONITOR_FILTER_SERVICE = 0x21D6,
      O_SERVER_MONITOR_SERVICE_LIST = 0x21D7,
      O_SERVER_MONITOR_FILTER_EVENT = 0x21D8,
		O_BCS_SERVER = 0x21D9,
		O_SERVER_MONITOR_LOG_PARSER_EVENT_POLL = 0x21DA,
		O_SERVER_MONITOR_LOG_PARSER_STATUS_POLL = 0x21DB,
		O_SOFTWARE_PACKAGE = 0x21DC,
		// = 0x21DD,
		O_SCRIPT_BC_SCRIPT = 0x21DE,
		O_SCRIPT_BC_SCRIPT_SET = 0x21DF,
		O_SCRIPT_BC_SCRIPT_TO_SCRIPT_SET = 0x21E0,
		O_SCRIPT_BC_TYPES = 0x21E1,
		O_QOS_BC_INTERFACE = 0x21E2,
      O_IMPORT_SERVER_IMPORT_CONFIG = 0x21E3,
      O_IMPORT_SERVER_IMPORT_FILE = 0x21E4,
      O_IMPORT_SERVER_MERGE_ALGORITHM = 0x21E5,
      O_IMPORT_SERVER_IMPORT_CONFIG_PARAMS = 0x21E6,
      O_SOURCE = 0x21E7,
      O_SUBTYPE_SOURCE = 0x21E8,
      O_SUBTYPE_PROP_SOURCE = 0x21E9,
      O_NODE_SOURCE = 0x21EA,
      O_GROUP_SOURCE = 0x21EB,
      O_MOD_SOURCE = 0x21EC,
   };

   enum _NODE_PROP_TYPE
   {
      // Node property types
      P_AGENT			= 0x2000,
      P_AGENTREF		= 0x2001,
      P_NOTOPOLOGY	= 0x2002,
      P_AUTONOTOPOLOGY	= 0x2003,
      P_NODISCOVERY	= 0x2004,
      P_ROUTERDEV		= 0x2005,
      P_SWITCHDEV		= 0x2006,
      P_HUBDEV			= 0x2007,
      P_DYNAMICADDRESS = 0x2008,
      P_BROWSEDEV		= 0x2009,
      P_PINGRETRY		= 0x200A,
      P_POLLDISABLE	= 0x200B,
      P_HTTP			= 0x200C,
      P_TCP			= 0x200D,
      P_WINNET		= 0x200E,
      P_AUTOICON		= 0x200F,
      P_SNMPOVERHTTPDEV	= 0x2010,
      P_SYSINFO		= 0x2011,
      P_CUSTOMINFO	= 0x2012,
      P_PING			= 0x2013,
      P_POLL_ON_DEMAND	= 0x2014,
      P_WMI			= 0x2015,
      P_BSM			= 0x2016,


      // Address property types
      P_SUBNETBITS	= 0x2061,

      // Poll property types
      P_POLLDETAILS   = 0x2080,
      P_LASTRESPONSETIME = 0x2081,
      P_STATUSPOLL    = 0x2082,
      P_NOTIFYREENTRY = 0x2083,
      P_NOTIFYALWAYS	= 0x2084,
      P_AVAILCONFIG	= 0x2085,
      P_RESPTIMECONFIG = 0x2086,

      // SnmpPoll property types
      P_DEFMIBVAR		= 0x20A0,

      // HttpPoll property types
      P_CONTFLAG		= 0x20C3,
      P_CONTTEXT		= 0x20C4,
      P_LOADPAGE		= 0x20C5,
      P_LOADSOURCES	= 0x20C6,
      P_LOADALLSOURCES = 0x20C7,
      P_CHECKLINKS	= 0x20C8,
      P_USEWININET	= 0x20C9,
      P_DEFHEADER		= 0x20CA,
      P_DEFUSEPROXY	= 0x20CB,
      P_DEFLOGIN		= 0x20CC,

      // TcpPoll property types

      // WinNetPoll property types
      P_SHARE			= 0x2100,

      // Subtype property types
      P_SUBTYPEID1	= 0x2120,
      P_SUBTYPEID2	= 0x2121,
      P_SUBTYPEPARAM	= 0x2122,
      P_SUBTYPE_PROP	= 0x2123,
      P_START_DUAL_SUBTYPE_PROP = 0x2124,
      P_END_DUAL_SUBTYPE_PROP = 0x2125,

      // Managed Objects
      P_MANAGED_OBJECT	= 0x2130,
      P_MO = P_MANAGED_OBJECT,
      P_MANAGED_OBJECT_ID	= 0x2131,
      P_MANAGED_OBJECT_ID1= 0x2132,
      P_MANAGED_OBJECT_ID2= 0x2133,
      P_MANAGED_OBJECT_PARAM = 0x2134,
      P_MANAGED_OBJECTREF	= 0x2135,

      P_ACT_DATE		= 0x2140,

      P_INTVAL1		= 0x2150,
      P_INTVAL2		= 0x2151,
      P_STRINGVAL		= 0x2152,
      P_BROWSE		   = 0x2153,
      P_SNMPOVERHTTP	= 0x2154,

      // System/CustomInfo property types
      P_CONTACT		= 0x2155,
      P_CUSTOM1		= 0x2156,
      P_CUSTOM2		= 0x2157,
      P_CUSTOM3		= 0x2158,

      P_STARTRELATION = 0x2159,
      P_ENDRELATION  = 0x215A,

      P_ERMGROUP		= 0x215B,
      P_CONTEXT		= 0x215C,
      P_SECURITYTYPE	= 0x215D,
      P_SECURITYMODEL = 0x215E,
      P_AUTHPROTOCOL	= 0x215F,
      P_AUTHPASSWORD	= 0x2160,
      P_PRIVPROTOCOL	= 0x2161,
      P_PRIVPASSWORD	= 0x2162,

      P_ENGINEID		= 0x2163,
      P_OLDENGINEID	= 0x2164,
      P_CHANGECOUNT	= 0x2165,

      P_OID				= 0x2166,
      P_LOGIN			= 0x2167,

      // Extended request properties
      P_DEFEXTREQUEST	= 0x2168,
      P_EXTREQUEST	= 0x2169,
      P_DEFREQTIME	= 0x216A,
      P_REQTIME		= 0x216B,
      P_REQDELAY		= 0x216C,
      P_DEFREQCOUNT	= 0x216D,
      P_REQCOUNT		= 0x216E,
      P_LASTREQCOUNT	= 0x216F,

      P_DEFVERSION	= 0x2170,		// Default SNMP version flag
      P_POLLING_INTERVAL	= 0x2171,
      P_HISTORY_INTERVAL	= 0x2172,
      P_RESPTIME_INTERVAL	= 0x2173,
      P_POLLING_TYPE = 0x2174,
      P_CLITYPE		= 0x2175,
      P_PROPERTYREF	= 0x2176,
      P_THRESHOLDREF = 0x2177,

      P_CORRELATION   = 0x2178,
      P_CORR_RULE     = 0x2179,
      P_CORR_FUNCTION = 0x217A,
      P_CORR_VALUE    = 0x217B,
      P_CORR_OBJECT   = 0x217C,

      P_STATUS_TIMESTAMP = 0x217D,
      P_VALUE_TIMESTAMP  = 0x217E,
      P_RESERVATIONREF   = 0x217F,

      P_GE_SUBTYPE	= 0x2180,
      P_GE_NODE		= 0x2181,

      P_PROVISIONING	= 0x2182,
      P_REFCONFIG		= 0x2183,

      P_KEEP_RESERVATION = 0x2184,
      P_ADDRESSNETREF	= 0x2185,
      P_ADDRESSRES		= 0x2186,

      P_TRANSPARENT		= 0x2187,
      P_TRANSPARENT_COLOLR = 0x2188,

      P_AVAILMODE       = 0x2189,
      P_AVAILTHRESHOLD  = 0x218A,

      P_NATTING  = 0x218B,
      P_VIRTUALSITE = 0x218C,
      P_NAT_START = 0x218D,
      P_NAT_END = 0x218E,
      P_NAT_TYPE = 0x218F,
      P_XML_FILE = 0x2190,
      P_MANAGED_OBJECT_TYPE = 0x2191,
      P_MO_TYPE = P_MANAGED_OBJECT_TYPE,
      P_NODE_TEMPLATE = 0x2192,
      P_MO_TEMPLATE = 0x2193,
      P_POLL_TEMPLATE = 0x2194,
      P_TEMPLATE_TYPE = 0x2195,
      P_TEMPLATE_ID = 0x2196,
      P_POLL_TEMPLATE_DISTRIBUTION = 0x2197,
      P_TR_CLASS = 0x2198,
      P_TR_GROUP = 0x2199,
      P_TR_NODE = 0x219A,
      P_TR_MO = 0x219B,
      P_TR_NODE_TEMPLATE = 0x219C,
      P_TR_MO_TEMPLATE = 0x219D,
      P_INVALID_CONFIG = 0x219F,
      P_GE = 0x21A0,
      P_CALL = 0x21A1,
      P_SCRIPT = 0x21A2,
      P_TASKITEM = 0x21A3,
      P_PROVISIONING_LOG = 0x21A4,
      P_PROVISIONING_NODE = 0x21A5,
      P_ROUTING = 0x21A6,
      P_MPLS = 0x21A7,
      P_EXECUTOR = 0x21A8,
      P_MODCMDB = 0x21A9,
      P_NODEGROUP = 0x21AA,
      P_PLUGIN = 0x21B0,
      P_ADAPTER = 0x21B1,
      P_SUBAGENT = 0x21B2,
      P_IGNORECOUNT = 0x21B3,

      P_DASHBOARD_PARAMETER = 0x21B4,
      P_XML = 0x21B5,

      P_BSM_EVENT_CATEGORY = 0x21B6,
      P_BSM_EVENT_CLASS = 0x21B7,
      P_BSM_EVENT_CLASSES = 0x21B8,
      P_BSM_EVENT_CATEGORIES = 0x21B9,
      P_BSM_ELEMENT = 0x21BA,

      P_DELETE_NODES = 0x21BB,

      // BSM_FILTER properties
      P_BSM_FILTER_ID = 0x21C0,
      P_BSM_CONNECTOR_TYPE = 0x21C1,
      P_BSM_PREPEND_EVENT_ID = 0x21C2,
      P_BSM_PREPEND_CAT_CLS = 0x21C3,
      P_BSM_FILTER_CONNECTOR_ADAPTERS = 0x21C4,
      P_BSM_FILTER_RULES = 0x21C5,

      // BSM_FILTER_RULE properties
      P_BSM_EVENT_TARGET = 0x21D0,
      P_BSM_SAT_NAME = 0x21D1,
      P_BSM_SAT_REF_TYPE = 0x21D2,
      P_BSM_MOT_NAME = 0x21D3,
      P_BSM_MOT_REF_TYPE = 0x21D4,
      P_BSM_CAT_NAME = 0x21D5,
      P_BSM_CAT_REF_TYPE = 0x21D6,
      P_BSM_CLS_NAME = 0x21D7,
      P_BSM_CLS_REF_TYPE = 0x21D8,

      P_CUSTOMTITLE = 0x21D9,
      P_TTL = 0x21DA,
      P_BROADCAST = 0x21DB,
      P_SERVER_MONITOR = 0x21DC,
      P_SERVER_MONITOR_XML = 0x21DD,
      P_NODE_PATH = 0x21DE,
      P_SINGLE_PATH = 0x21DF,
      P_SINGLE_PATH_ELEMENT = 0x21E0,
      P_CORRMODE = 0x21E1,
      P_DISCOVERY = 0x21E2,

      P_CI_OBJECT_TYPE_1 = 0x21F0,
      P_CI_OBJECT_ID_1 = 0x21F1,
      P_CI_OBJECT_TYPE_2 = 0x21F2,
      P_CI_OBJECT_ID_2 = 0x21F3,

      P_SM_TEMPLATE = 0x21F4,
      P_IMPORTED = 0x21F6,
      P_QUERY_TYPE = 0x21F7,
      P_START_MODE = 0x21F8,
      P_AUTO_RESTART = 0x21F9,
      P_FATAL_STATUS = 0x21FA,
      P_NORMAL_STATUS = 0x21FB,
      P_ERROR_EVENT = 0x21FC,
      P_INFO_EVENT = 0x21FD,
      P_PROCESS_NAME = 0x21FE,
      P_EVENT_TYPE = 0x21FF,
      P_LOG_NAME = 0x2200,
      P_SOURCE_NAME = 0x2201,
      P_POLL_INTERVALL = 0x2202,
      P_THRESHOLD_COUNTER = 0x2203,
      P_DISK_PARTITION_NAME = 0x2204,
      P_LOG_TYPE = 0x2205,
      P_RESET_THRESHOLD = 0x2206,
      P_ENABLE_BLACKLIST = 0x2207,

      P_SERVER_MONITOR_SERVICE_POLL = 0x220F,
      P_SERVER_MONITOR_PROCESS_POLL = 0x2210,
      P_SERVER_MONITOR_EVENT_LOG_POLL = 0x2211,
      P_SERVER_MONITOR_PROCESSOR_POLL = 0x2212,
      P_SERVER_MONITOR_DISK_POLL = 0x2213,
      P_SERVER_MONITOR_MEMORY_POLL = 0x2214,
      P_SERVER_MONITOR_EVENT_LOG_STATUS_MAPPING = 0x2215,
      P_SERVER_MONITOR_THRESHOLD_MAPPING = 0x2216,
      P_POLL_REF = 0x2217,
      P_CATEGORY_TYPE = 0x2218,
      P_SERVICE_STATUS = 0x2219,
      P_PROCESS_STATUS = 0x221A,
      P_SERVER_MONITOR_SERVICE_STATUS_MAPPING = 0x221B,
      P_SERVER_MONITOR_PROCESS_STATUS_MAPPING = 0x221C,
      P_LOGPARSER = 0x221D,
      P_LINES_BEFORE = 0x221E,
      P_LINES_AFTER = 0x222F,
      P_DATA_SOURCE_ID = 0x2230,
      P_DATA_SOURCE_NAME = 0x2231,
      P_LAST_POSITION = 0x2232,
      P_LAST_DATA_RECEIVED = 0x2233,
      P_LAST_POSITION_TYPE = 0x2234,
      //Custom Fields expanded
      P_CUSTOM4 = 0x2235,
      P_CUSTOM5 = 0x2236,
      P_SERVER_MONITOR_FILTER_SERVICE = 0x2237,
      P_SERVER_MONITOR_FILTER_EVENT = 0x2238,
      P_LICENSE_STATUS = 0x2239,
      P_SERVER_MONITOR_LOG_PARSER_EVENT_POLL = 0x223A,
      P_SERVER_MONITOR_LOG_PARSER_STATUS_POLL = 0x223B,
      P_SM_REFERENCE = 0x223C,
      P_TEXT_THRESHOLD = 0x223D,
      P_ENTITY_REFERENCE = 0x223E,
      P_SOFTWARE_PACKAGE = 0x223F,
      P_CALL_BACK_HOST = 0x2240,
      P_DNSNAME		= 0x2241,
      P_POLLTYPE		= 0x2243,
      P_CREATETIME	= 0x2244,
      P_LASTCHANGED	= 0x2245,
      P_LASTFOUND		= 0x2246,
      P_LASTACTIVE	= 0x2247,
      P_AUTODELETE	= 0x2248,
      P_INVTYPE		= 0x2249,
      P_AGENTNAME		= 0x224A,
      P_SERIAL_NUMBER = 0x224B,
      P_INSTALL_DATE = 0x224C,
      P_SOFTWARE_INSTALLED = 0x224D,
      P_SCRIPT_CONTENT = 0x224E,
      P_PERFORMANCE_DATA = 0x224F,
      P_HAS_COLUMN_HEADERS = 0x2250,
      P_KEY_COLUMN_ID = 0x2251,
      P_RELEVANT_COLUMNS = 0x2252,
      P_COLUMN_SEPARATOR = 0x2253,
      P_PROPERTYNAME_PREFIX = 0x2254,
      P_SYSTEM_TYPE = 0x2255,
      P_SYSTEM_VERSION = 0x2256,
      P_SUBAGENT_TYPE = 0x2257,
      P_SCRIPT_SET = 0x2258,
      P_SCRIPT_TO_SCRIPT_SET = 0x2259,
      P_EXPORTED = 0x225A,
      P_PARSER_TYPE = 0x225B,
      P_QOS_BC_INTERFACE = 0x225C,
      P_CENTRAL_SITE = 0x225D,
      P_IMPORT_FILE_REF = 0x225E,
      P_ALGORITHM = 0x225F,
      P_CONVERTER_FILE = 0x2260,
      P_IMPORT_CONFIG_REF = 0x2261,
      P_ENCODING = 0x2262,
   };

	enum SoftwarePackageType
	{
		SPT_NONE = 0,
		SPT_PACKAGE = 1,
		SPT_HOTFIX = 2
	};
	#define SOFTWARE_PACKAGE_TYPE_ARRAY(name) std::pair<int, std::string> name [] = {\
	std::make_pair(SPT_NONE, "None"),\
	std::make_pair(SPT_PACKAGE, "Package"),\
	std::make_pair(SPT_HOTFIX, "Hotfix"),\
      };

	enum ScriptBcTypes
	{
		SBCT_SUBAGENT = 0,
		SBCT_MOT = 1
	};
	#define SCRIPT_BC_TYPE_ARRAY(name) std::pair<int, std::string> name [] = {\
	std::make_pair(SBCT_SUBAGENT, "Subagent"),\
	std::make_pair(SBCT_MOT, "MOT"),\
      };

#define SCRIPT_BC_TYPE_MAP(mapname,arrayname) std::map<int, std::string> mapname (arrayname, arrayname + sizeof (arrayname) / sizeof (std::pair<int, std::string>));




} // namespace CINEMA

#endif // _CinemaSONode_H_
