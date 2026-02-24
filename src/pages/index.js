import { useEffect } from 'react';
import { useLocation } from '@docusaurus/router';

export default function Home() {
  const location = useLocation();
  useEffect(() => {
    window.location.replace('/docs/intro');
  }, []);
  return null;
}
