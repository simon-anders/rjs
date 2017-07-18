library( httpuv )
library( jsonlite )

websocket <- NULL

webApp <- list(
   
   call = function( req ) {
      list(
         status = 200L,
         headers = list( 'Content-Type' = 'text/html' ),
         body = paste( readLines( "page.html" ), collapse="\n" )
      )
   },
   
   onWSOpen = function( ws ) {
      if( ws$request$HTTP_SEC_WEBSOCKET_PROTOCOL != "RJS-0" )
         stop( paste( "Unknown WebSocket protocol", ws$request$HTTP_SEC_WEBSOCKET_PROTOCOL ) )
      ws$onMessage( function( isBinary, msg ) {
         if( isBinary )
            stop( "Error: Binary message received" )
         msg <- fromJSON( msg )
         if( msg[1] == "REPLY_JS" ) {
            result_callbacks[[ msg[2] ]]( msg[[3]] )
         }
      } );
      websocket <<- ws
   }
   
)

max_result_handle <- 0L

result_callbacks <- new.env()

evaljs <- function( jscode, onresult=NULL ) {
   if( !is.null(onresult) ) {
      max_result_handle <<- max_result_handle + 1
      result_handle <- as.character( max_result_handle )
      result_callbacks[[ result_handle ]] <- onresult
   } else {
      result_handle <- -1L
   }
   websocket$send( sprintf( "[\"EVAL_JS\", %s, %s ]", result_handle, deparse(jscode) ) )
}

if( !is.null(server) ){
   stopDaemonizedServer( server )
   server <- NULL
   websocket <- NULL
}
server <- startDaemonizedServer( "0.0.0.0", 1235, webApp )
getOption("viewer")( "http://localhost:1235" )
while( is.null(websocket) ) {
   Sys.sleep( .05 )
}

evaljs( '3+15', print ) 

#stopDaemonizedServer( server )
#server <- NULL
#websocket <- NULL
