use std::error;
use std::fmt;

pub type Result<T> = std::result::Result<T, Box<dyn error::Error>>;

#[derive(Debug)]
pub enum ConnectionError {
    Unknown,
    IncompatibleTypes(/*from*/ String, /*to*/ String),
    AlreadyConnected,
    NotConnected,
    Other(Box<dyn error::Error>),
}


#[derive(Debug)]
pub enum BTrackError{
    Unknown,
    ConnectionError(ConnectionError),
    Other(Box<dyn error::Error>),
}

impl fmt::Display for BTrackError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BTrackError::Unknown =>
                write!(f, "Unknown error"),
            BTrackError::ConnectionError(e) =>
                write!(f, "{}", e),
            BTrackError::Other(e) =>
                e.fmt(f),
        }
    }
}

impl fmt::Display for ConnectionError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ConnectionError::Unknown =>
                write!(f, "Unknown error"),
            ConnectionError::IncompatibleTypes(from, to) =>
                write!(f, "Incompatible types: cannot convert from {} to {}", from, to),
            ConnectionError::AlreadyConnected =>
                write!(f, "Already connected"),
            ConnectionError::NotConnected =>
                write!(f, "Not connected"),
            ConnectionError::Other(e) =>
                e.fmt(f),
        }
    }
}

impl error::Error for BTrackError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            BTrackError::Unknown => None,
            BTrackError::ConnectionError(e) => Some(e),
            BTrackError::Other(e) => Some(e.as_ref()),
        }
    }
}

impl error::Error for ConnectionError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            ConnectionError::Unknown => None,
            ConnectionError::AlreadyConnected => None,
            ConnectionError::IncompatibleTypes(_, _) => None,
            ConnectionError::NotConnected => None,
            ConnectionError::Other(e) => Some(e.as_ref()),
        }
    }
}