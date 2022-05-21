module Name where

import Data.Text (Text)
import Lens.Micro (Lens', lens)
import Lib

-- | A person's given names(s). There may be more than one name present, sep. by spaces.
newtype GivenName = GivenName Text deriving (Eq, Show, Ord)

class HasGivenName x where
  givenName :: Lens' x GivenName

instance HasGivenName Name where
  givenName = lens _givenName $ \n q -> n {_givenName = q}

-- | A person's family name(s). There may be more than one name present, sep. by spaces.
newtype Surname = Surname Text deriving (Eq, Ord, Show)

class HasSurname x where
  surname :: Lens' x Surname

instance HasSurname Name where
  surname = lens _surname $ \n q -> n {_surname = q}

-- | A person's prefix (Sir, Miss)
newtype NamePrefix = NamePrefix (Maybe Text) deriving (Eq, Ord, Show)

class HasNamePrefix x where
  nameNamePrefix :: Lens' x NamePrefix

-- | A person's suffix, (Jr, III)
newtype NameSuffix = NameSuffix (Maybe Text) deriving (Eq, Ord, Show)

class HasNameSuffix x where
  nameSuffix :: Lens' x NameSuffix

instance HasNameSuffix Name where
  nameSuffix = lens _nameSuffix $ \n q -> n {_nameSuffix = q}

data Name = Name
  { _namePrefix :: !NamePrefix,
    _givenName :: !GivenName,
    _surname :: !Surname,
    _nameSuffix :: !NameSuffix
  }
  deriving (Eq, Show, Ord)
